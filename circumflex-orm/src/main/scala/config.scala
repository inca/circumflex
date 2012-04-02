package ru.circumflex
package orm

import core._
import javax.sql.DataSource
import javax.naming.InitialContext
import java.util.Date
import java.sql.{Timestamp, Connection, PreparedStatement}
import com.mchange.v2.c3p0.{DataSources, ComboPooledDataSource}
import scala.xml._

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

  // Configuration identification

  def name: String
  def prefix(sym: String) = if (name == "") "" else name + sym

  // Database connectivity parameters

  lazy val url = cx.getString("orm.connection.url")
      .getOrElse(throw new ORMException(
    "Missing mandatory configuration parameter 'orm.connection.url'."))
  lazy val username = cx.getString("orm.connection.username")
      .getOrElse(throw new ORMException(
    "Missing mandatory configuration parameter 'orm.connection.username'."))
  lazy val password = cx.getString("orm.connection.password")
      .getOrElse(throw new ORMException(
    "Missing mandatory configuration parameter 'orm.connection.password'."))

  lazy val dialect = cx.instantiate[Dialect]("orm.dialect", url match {
    case u if (u.startsWith("jdbc:postgresql:")) => new PostgreSQLDialect
    case u if (u.startsWith("jdbc:mysql:")) => new MySQLDialect
    case u if (u.startsWith("jdbc:oracle:")) => new OracleDialect
    case u if (u.startsWith("jdbc:h2:")) => new H2Dialect
    case u if (u.startsWith("jdbc:sqlserver:")) => new MSSQLDialect
    case u if (u.startsWith("jdbc:db2:")) => new DB2Dialect
    case _ => new Dialect
  })

  lazy val driver = cx.get("orm.connection.driver") match {
    case Some(s: String) => s
    case _ => dialect.driverClass
  }

  lazy val isolation: Int = cx.get("orm.connection.isolation") match {
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

  // Configuration objects

  lazy val typeConverter: TypeConverter = cx.instantiate[TypeConverter](
    "orm.typeConverter", new TypeConverter)
  lazy val transactionManager: TransactionManager = cx.instantiate[TransactionManager](
    "orm.transactionManager", new DefaultTransactionManager)
  lazy val defaultSchema: Schema = new Schema(
    cx.get("orm.defaultSchema").map(_.toString).getOrElse("public"))
  lazy val statisticsManager: StatisticsManager = cx.instantiate[StatisticsManager](
    "orm.statisticsManager", new StatisticsManager)
  lazy val connectionProvider: ConnectionProvider = cx.instantiate[ConnectionProvider](
    "orm.connectionProvider", new SimpleConnectionProvider(driver, url, username, password, isolation))

  // Override to forbid `executeUpdate` statements on configuration-level
  def readOnly = false
}

class SimpleORMConfiguration(val name: String) extends ORMConfiguration

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

     * `orm.connection.url`
     * `orm.connection.username`
     * `orm.connection.password`

  * set _auto-commit_ for each connection to `false`
  * set the transaction isolation level to the value `orm.connection.isolation`
  (or use `READ COMMITTED` by default)

 If c3p0 data source is used you can fine tune it's configuration with `c3p0.properties`
 file (see [c3p0 documentation][c3p0-cfg] for more details).

 Though `SimpleConnectionProvider` is an optimal choice for most applications, you
 can create your own connection provider by implementing the `ConnectionProvider` trait
 and setting the `orm.connectionProvider` configuration parameter.

   [c3p0]: http://www.mchange.com/projects/c3p0
   [c3p0-cfg]: http://www.mchange.com/projects/c3p0/index.html#configuration_properties
*/
class SimpleConnectionProvider(
                                  val driverClass: String,
                                  val url: String,
                                  val username: String,
                                  val password: String,
                                  val isolation: Int)
    extends ConnectionProvider {

  protected def createDataSource: DataSource = cx.get("orm.connection.datasource") match {
    case Some(jndiName: String) => {
      val ctx = new InitialContext
      val ds = ctx.lookup(jndiName).asInstanceOf[DataSource]
      ORM_LOG.info("Using JNDI datasource ({}).", jndiName)
      ds
    }
    case _ => {
      ORM_LOG.info("Using c3p0 connection pool.")
      val ds = new ComboPooledDataSource()
      ds.setDriverClass(driverClass)
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
    conn.setAutoCommit(false)
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
      case ba: Array[Byte] => st.setBytes(paramIndex, ba)
      case v => st.setObject(paramIndex, v)
    }
  }
}