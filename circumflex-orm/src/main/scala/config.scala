package ru.circumflex.orm

import com.mchange.v2.c3p0.ComboPooledDataSource
import ru.circumflex.core.Circumflex
import java.sql.{Timestamp, PreparedStatement, ResultSet, Connection}
import java.util.Date
import javax.naming.InitialContext
import javax.sql.DataSource
import org.slf4j.LoggerFactory
import ORM._

/* ## Configuration */

/**
 * `ORM` singleton aggregates all ORM-related interfaces into a single
 * configuration object.
 */
object ORM {

  protected[orm] val ormLog = LoggerFactory.getLogger("ru.circumflex.orm")

  /**
   * Connection provider.
   * Can be overriden with `orm.connectionProvider` configuration parameter.
   */
  val connectionProvider: ConnectionProvider = Circumflex.cfg("orm.connectionProvider") match {
    case Some(p: ConnectionProvider) => p
    case Some(c: Class[ConnectionProvider]) => c.newInstance
    case Some(s: String) => Class
        .forName(s, true, Circumflex.classLoader)
        .newInstance
        .asInstanceOf[ConnectionProvider]
    case _ => DefaultConnectionProvider
  }

  /**
   * SQL dialect.
   * Can be overriden with `orm.dialect` configuration parameter.
   */
  val dialect: Dialect = Circumflex.cfg("orm.dialect") match {
    case Some(d: Dialect) => d
    case Some(c: Class[Dialect]) => c.newInstance
    case Some(s: String) => Class.forName(s, true, Circumflex.classLoader)
        .newInstance
        .asInstanceOf[Dialect]
    case _ => DefaultDialect
  }

  /**
   * SQL type converter.
   * Can be overriden with `orm.typeConverter` configuration parameter.
   */
  val typeConverter: TypeConverter = Circumflex.cfg("orm.typeConverter") match {
    case Some(tc: TypeConverter) => tc
    case Some(c: Class[TypeConverter]) => c.newInstance
    case Some(s: String) => Class.forName(s, true, Circumflex.classLoader)
        .newInstance
        .asInstanceOf[TypeConverter]
    case _ => DefaultTypeConverter
  }

  /**
   * The schema name which is used if not specified explicitly.
   * Can be overriden with `orm.defaultSchema` configuration parameter.
   */
  val defaultSchemaName = Circumflex.cfg("orm.defaultSchema") match {
    case Some(s: String) => s
    case _ => "public"
  }

  /**
   * Transaction manager.
   * Can be overriden with `orm.transactionManager` configuration parameter.
   */
  val transactionManager: TransactionManager = Circumflex.cfg("orm.transactionManager") match {
    case Some(tm: TransactionManager) => tm
    case Some(c: Class[TransactionManager]) => c.newInstance
    case Some(s: String) => Class.forName(s, true, Circumflex.classLoader)
        .newInstance
        .asInstanceOf[TransactionManager]
    case _ => DefaultTransactionManager
  }

  /**
   * Shortcut for retrieving current transaction via `transactionManager.getTransaction`.
   */
  def tx = transactionManager.getTransaction
}

/* ### Connection provider */

/**
 * *Connection provider* is used to acquire JDBC connections throughout the application.
 */
trait ConnectionProvider {

  /**
   * Open new JDBC connection.
   */
  def openConnection: Connection

}

/**
 * Default `ConnectionProvider` implementation. It behaves as follows:
 *
 *  * if `orm.connection.datasource` is set, use it to acquire data source
 *  from JNDI;
 *  * if `orm.connection.datasource` is missing, construct a connection
 *  pool using [c3p0][] and following configuration parameters:
 *     * `orm.connection.driver`
 *     * `orm.connection.url`
 *     * `orm.connection.username`
 *     * `orm.connection.password`
 *   * set *auto-commit* for each connection to `false`
 *   * set the transaction isolation level to the value `orm.connection.isolation`
 *   (or use `READ COMMITTED` by default)
 *
 * If c3p0 data source is used you can fine tune it's configuration with `c3p0.properties`
 * file (see [c3p0 documentation][c3p0-cfg] for more details).
 *
 * Though `DefaultConnectionProvider` is an optimal choice for most applications, you
 * can create your own connection provider by implementing the `ConnectionProvider` trait
 * and setting the `orm.connectionProvider` configuration parameter.
 *
 *   [c3p0]: http://www.mchange.com/projects/c3p0
 *   [c3p0-cfg]: http://www.mchange.com/projects/c3p0/index.html#configuration_properties
 */
class DefaultConnectionProvider extends ConnectionProvider {

  protected val isolation: Int = Circumflex.cfg("orm.connection.isolation") match {
    case Some("none") => Connection.TRANSACTION_NONE
    case Some("read_uncommitted") => Connection.TRANSACTION_READ_UNCOMMITTED
    case Some("read_committed") => Connection.TRANSACTION_READ_COMMITTED
    case Some("repeatable_read") => Connection.TRANSACTION_REPEATABLE_READ
    case Some("serializable") => Connection.TRANSACTION_SERIALIZABLE
    case _ => {
      ormLog.info("Using READ COMMITTED isolation, override 'orm.connection.isolation' if necesssary.")
      Connection.TRANSACTION_READ_COMMITTED
    }
  }

  /**
   * Configure datasource instance. It is retrieved from JNDI if 'orm.connection.datasource'
   * is specified or is constructed using c3p0 otherwise.
   */
  protected val ds: DataSource = Circumflex.cfg("orm.connection.datasource") match {
    case Some(jndiName: String) => {
      val ctx = new InitialContext
      val ds = ctx.lookup(jndiName).asInstanceOf[DataSource]
      ormLog.info("Using JNDI datasource ({}).", jndiName)
      ds
    }
    case _ => {
      ormLog.info("Using c3p0 connection pooling.")
      val driver = Circumflex.cfg("orm.connection.driver") match {
        case Some(s: String) => s
        case _ => throw new ORMException("Missing mandatory configuration parameter 'orm.connection.driver'.")
      }
      val url = Circumflex.cfg("orm.connection.url") match {
        case Some(s: String) => s
        case _ => throw new ORMException("Missing mandatory configuration parameter 'orm.connection.url'.")
      }
      val username = Circumflex.cfg("orm.connection.username") match {
        case Some(s: String) => s
        case _ => throw new ORMException("Missing mandatory configuration parameter 'orm.connection.username'.")
      }
      val password = Circumflex.cfg("orm.connection.password") match {
        case Some(s: String) => s
        case _ => throw new ORMException("Missing mandatory configuration parameter 'orm.connection.password'.")
      }
      val ds = new ComboPooledDataSource()
      ds.setDriverClass(driver)
      ds.setJdbcUrl(url)
      ds.setUser(username)
      ds.setPassword(password)
      ds
    }
  }

  def dataSource: DataSource = ds

  /**
   * Open a new JDBC connection.
   */
  def openConnection: Connection = {
    val conn = dataSource.getConnection
    conn.setAutoCommit(false)
    conn.setTransactionIsolation(isolation)
    return conn
  }

}

object DefaultConnectionProvider extends DefaultConnectionProvider

/* ### Type converter */

/**
 * *Type converters* are used to read atomic values from JDBC result sets and to set
 * JDBC prepared statement values for execution. If you intend to use custom types,
 * provide your own implementation.
 */
trait TypeConverter {

  /**
   * Read a value from specified `ResultSet` at specified column `alias`.
   */
  def read(rs: ResultSet, alias: String): Option[Any] = {
    val result = rs.getObject(alias)
    if (rs.wasNull) return None
    else return Some(result)
  }

  /**
   * Write a value to specified `PreparedStatement` at specified column `paramIndex`.
   */
  def write(st: PreparedStatement, parameter: Any, paramIndex: Int): Unit = parameter match {
    case Some(p) => write(st, p, paramIndex)
    case None | null => st.setObject(paramIndex, null)
    case value => st.setObject(paramIndex, convert(value))
  }

  /**
   * Convert a value.
   */
  def convert(value: Any): Any = value match {
    case (p: Date) => new Timestamp(p.getTime)
    case value => value
  }

  /**
   * Convert a value and return it literally (with quoting strings).
   */
  def toString(value: Any): String = convert(value) match {
    case None | null => "null"
    case s: String => dialect.quoteLiteral(s)
    case d: Timestamp => dialect.quoteLiteral(d.toString)
    case other => other.toString
  }
}

object DefaultTypeConverter extends TypeConverter
