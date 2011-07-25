package ru.circumflex
package orm

import core._
import javax.sql.DataSource
import javax.naming.InitialContext
import java.util.Date
import java.sql.{Timestamp, Connection, PreparedStatement}
import com.mchange.v2.c3p0.{DataSources, ComboPooledDataSource}
import collection.mutable.HashMap
import xml._
import util.control.ControlThrowable

/*!# ORM Configuration Objects

Circumflex ORM needs to know a little about your environment to operate.
Following objects configure different aspects of Circumflex ORM:

  * _connection provider_ is used to acquire JDBC connections, can be
  overriden using the `orm.connectionProvider` configuration parameter;
  * _type converter_ handles convertions between JDBC and Scala data types,
  can be overriden using the `orm.typeConverter` configuration parameter;
  * _dialect_ handles all SQL rendering throughout the framework,
  can be overriden using the `orm.dialect` configuration parameter;
  * _transaction manager_ is responsible for allocating current transactions
  for execution contexts.
*/

trait ORMConfiguration {
  def dialect: Dialect
  def connectionProvider: ConnectionProvider
  def typeConverter: TypeConverter
  def transactionManager: TransactionManager
  def defaultSchema: Schema
  def statisticsManager: StatisticsManager

  def cacheService = ctx.get("orm.contextCache") match {
    case Some(cs: CacheService) => cs
    case _ =>
      val cs = cx.instantiate[CacheService]("orm.contextCache", new DefaultCacheService)
      ctx.update("orm.contextCache", cs)
      cs
  }
}

class DefaultORMConfiguration extends ORMConfiguration {
  val dialect = cx.instantiate[Dialect]("orm.dialect", cx.get("orm.connection.url") match {
    case Some(url: String) =>
      if (url.startsWith("jdbc:postgresql:")) new PostgreSQLDialect
      else if (url.startsWith("jdbc:mysql:")) new MySQLDialect
      else if (url.startsWith("jdbc:oracle:")) new OracleDialect
      else if (url.startsWith("jdbc:h2:")) new H2Dialect
      else if (url.startsWith("jdbc:sqlserver:")) new MSSQLDialect
      else if (url.startsWith("jdbc:db2:")) new DB2Dialect
      else new Dialect
    case _ => new Dialect
  })
  val connectionProvider = cx.instantiate[ConnectionProvider](
    "orm.connectionProvider", new DefaultConnectionProvider)
  val typeConverter = cx.instantiate[TypeConverter](
    "orm.typeConverter", new TypeConverter)
  val transactionManager = cx.instantiate[TransactionManager](
    "orm.transactionManager", new DefaultTransactionManager)
  val defaultSchema = new Schema(
    cx.get("orm.defaultSchema").map(_.toString).getOrElse("public"))
  val statisticsManager = cx.instantiate[StatisticsManager](
    "orm.statisticsManager", new StatisticsManager)
}

/*!## Connection Provider

The `ConnectionProvider` is a simple trait responsible for acquiring JDBC
connections throughout the application.
*/
trait ConnectionProvider {
  def openConnection(): Connection
  def close()
}

/*! Circumflex ORM provides default `ConnectionProvider` implementation.
It behaves as follows:

  * if `orm.connection.datasource` is set, use it to acquire data source
  from JNDI;
  * if `orm.connection.datasource` is missing, construct a connection
  pool using [c3p0][] and following configuration parameters:

     * `orm.connection.driver`
     * `orm.connection.url`
     * `orm.connection.username`
     * `orm.connection.password`

  * set _auto-commit_ for each connection to `false`
  * set the transaction isolation level to the value `orm.connection.isolation`
  (or use `READ COMMITTED` by default)

 If c3p0 data source is used you can fine tune it's configuration with `c3p0.properties`
 file (see [c3p0 documentation][c3p0-cfg] for more details).

 Though `DefaultConnectionProvider` is an optimal choice for most applications, you
 can create your own connection provider by implementing the `ConnectionProvider` trait
 and setting the `orm.connectionProvider` configuration parameter.

   [c3p0]: http://www.mchange.com/projects/c3p0
   [c3p0-cfg]: http://www.mchange.com/projects/c3p0/index.html#configuration_properties
*/
class DefaultConnectionProvider extends ConnectionProvider {

  protected val autocommit: Boolean = cx.get("orm.connection.autocommit") match {
    case Some("true") => true
    case _ => false
  }

  protected val isolation: Int = cx.get("orm.connection.isolation") match {
    case Some("none") => Connection.TRANSACTION_NONE
    case Some("read_uncommitted") => Connection.TRANSACTION_READ_UNCOMMITTED
    case Some("read_committed") => Connection.TRANSACTION_READ_COMMITTED
    case Some("repeatable_read") => Connection.TRANSACTION_REPEATABLE_READ
    case Some("serializable") => Connection.TRANSACTION_SERIALIZABLE
    case _ => {
      ORM_LOG.info("Using READ COMMITTED isolation, override 'orm.connection.isolation' if necesssary.")
      Connection.TRANSACTION_READ_COMMITTED
    }
  }

  protected def createDataSource: DataSource = cx.get("orm.connection.datasource") match {
    case Some(jndiName: String) => {
      val ctx = new InitialContext
      val ds = ctx.lookup(jndiName).asInstanceOf[DataSource]
      ORM_LOG.info("Using JNDI datasource ({}).", jndiName)
      ds
    }
    case _ => {
      ORM_LOG.info("Using c3p0 connection pool.")
      val url = cx.get("orm.connection.url") match {
        case Some(s: String) => s
        case _ =>
          throw new ORMException("Missing mandatory configuration parameter 'orm.connection.url'.")
      }
      val driver = cx.get("orm.connection.driver") match {
        case Some(s: String) => s
        case _ => ormConf.dialect.driverClass
      }
      val username = cx.get("orm.connection.username") match {
        case Some(s: String) => s
        case _ =>
          throw new ORMException("Missing mandatory configuration parameter 'orm.connection.username'.")
      }
      val password = cx.get("orm.connection.password") match {
        case Some(s: String) => s
        case _ =>
          throw new ORMException("Missing mandatory configuration parameter 'orm.connection.password'.")
      }
      val ds = new ComboPooledDataSource()
      ds.setDriverClass(driver)
      ds.setJdbcUrl(url)
      ds.setUser(username)
      ds.setPassword(password)
      ds
    }
  }

  protected var _ds: DataSource = null
  def dataSource: DataSource = {
    if (_ds == null)
      _ds = createDataSource
    _ds
  }

  def openConnection(): Connection = {
    val conn = dataSource.getConnection
    conn.setAutoCommit(autocommit)
    conn.setTransactionIsolation(isolation)
    conn
  }

  def close() {
    DataSources.destroy(_ds)
    _ds = null
  }
}

/*!## Type converter

The `TypeConverter` trait is used to set JDBC prepared statement values for execution.
If you intend to use custom types, provide your own implementation.
*/
class TypeConverter {
  def write(st: PreparedStatement, parameter: Any, paramIndex: Int) {
    parameter match {
      case None | null => st.setObject(paramIndex, null)
      case Some(v) => write(st, v, paramIndex)
      case p: Date => st.setObject(paramIndex, new Timestamp(p.getTime))
      case x: Elem => st.setString(paramIndex, x.toString())
      case bd: BigDecimal => st.setBigDecimal(paramIndex, bd.bigDecimal)
      case v => st.setObject(paramIndex, v)
    }
  }
}

/*!# Transaction manager

No communication with the database can occur outside of a database transaction.

The `Transaction` class wraps JDBC `Connection` and provides simple methods
for committing or rolling back underlying transaction as well as for executing
various typical JDBC statements.

The `TransactionManager` trait is responsible for allocation current transaction
for application's execution context. The default implementation uses `Context`,
however, your application may require different approaches to transaction
demarcation -- in this case you may provide your own implementation.

JDBC `PreparedStatement` objects are also cached within `Transaction` for
performance considerations.
*/
class Transaction {

  // Connections are opened lazily
  protected var _connection: Connection = null

  // Statements are cached by actual SQL
  protected val _statementsCache = new HashMap[String, PreparedStatement]()

  def isLive: Boolean =
    _connection != null && !_connection.isClosed

  def commit() {
    if (isLive && !_connection.getAutoCommit) _connection.commit()
  }

  def rollback() {
    if (isLive && !_connection.getAutoCommit) _connection.rollback()
    ormConf.cacheService.invalidate()
  }

  def close() {
    if (isLive) try {
      // close all cached statements
      _statementsCache.values.foreach(_.close())
    } finally {
      // clear statements cache
      _statementsCache.clear()
      // close connection
      _connection.close()
      ormConf.statisticsManager.connectionsClosed.incrementAndGet()
      ORM_LOG.trace("Closed a JDBC connection.")
    }
  }

  protected def getConnection: Connection = {
    if (_connection == null || _connection.isClosed) {
      _connection = ormConf.connectionProvider.openConnection()
      ormConf.statisticsManager.connectionsOpened.incrementAndGet()
      ORM_LOG.trace("Opened a JDBC connection.")
    }
    _connection
  }

  def execute[A](connActions: Connection => A,
                 errActions: Throwable => A): A =
    try {
      ormConf.statisticsManager.executions.incrementAndGet()
      val result = connActions(getConnection)
      ormConf.statisticsManager.executionsSucceeded.incrementAndGet()
      result
    } catch {
      case e =>
        ormConf.statisticsManager.executionsFailed.incrementAndGet()
        errActions(e)
    }

  def execute[A](sql: String,
                 stActions: PreparedStatement => A,
                 errActions: Throwable => A): A = execute({ conn =>
    ORM_LOG.debug(sql)
    val st =_statementsCache.get(sql).getOrElse {
      val statement = ormConf.dialect.prepareStatement(conn, sql)
      _statementsCache.update(sql, statement)
      statement
    }
    stActions(st)
  }, errActions)

  def apply[A](block: => A): A = {
    val sp = getConnection.setSavepoint()
    try {
      block
    } catch {
      case e: ControlThrowable =>
        ORM_LOG.trace("Escaped nested transaction via ControlThrowable, ROLLBACK is suppressed.")
        throw e
      case e =>
        getConnection.rollback(sp)
        throw e
    } finally {
      getConnection.releaseSavepoint(sp)
    }
  }

}

trait TransactionManager {
  def get: Transaction
}

class DefaultTransactionManager extends TransactionManager {

  Context.addDestroyListener(c => try {
    get.commit()
    ORM_LOG.trace("Committed current transaction.")
  } catch {
    case e1: Exception =>
      ORM_LOG.error("Could not commit current transaction", e1)
      try {
        get.rollback()
        ORM_LOG.trace("Rolled back current transaction.")
      } catch {
        case e2: Exception =>
          ORM_LOG.error("Could not roll back current transaction", e2)
      }
  } finally {
    get.close()
  })

  def get: Transaction = ctx.get("orm.transaction") match {
    case Some(t: Transaction) => t
    case _ =>
      val t = cx.instantiate[Transaction]("orm.transaction", new Transaction)
      ctx.update("orm.transaction", t)
      t
  }

}
