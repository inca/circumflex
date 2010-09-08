package ru.circumflex.orm

import com.mchange.v2.c3p0.ComboPooledDataSource
import ru.circumflex.core._
import java.sql.{Timestamp, PreparedStatement, ResultSet, Connection}
import java.util.Date
import javax.naming.InitialContext
import javax.sql.DataSource
import ORM._
import org.slf4j.LoggerFactory

// ## Configuration

/**
 * `ORM` singleton aggregates all ORM-related interfaces into a single
 * configuration object.
 */
object ORM {

  protected[orm] val ormLog = LoggerFactory.getLogger("ru.circumflex.orm")

  // ### Global Configuration Objects

  /**
   * Connection provider.
   * Can be overriden with `orm.connectionProvider` configuration parameter.
   */
  val connectionProvider: ConnectionProvider = cx.instantiate[ConnectionProvider](
    "orm.connectionProvider", DefaultConnectionProvider)

  /**
   * SQL dialect.
   * Can be overriden with `orm.dialect` configuration parameter.
   */
  val dialect: Dialect = cx.instantiate[Dialect]("orm.dialect", cx.get("orm.connection.url") match {
      case Some(url: String) =>
        if (url.startsWith("jdbc:postgresql:")) new PostgreSQLDialect
        else if (url.startsWith("jdbc:mysql:")) new MySQLDialect
        else if (url.startsWith("jdbc:oracle:")) new OracleDialect
        else DefaultDialect
      case _ => DefaultDialect
    })

  /**
   * SQL type converter.
   * Can be overriden with `orm.typeConverter` configuration parameter.
   */
  val typeConverter: TypeConverter = cx.instantiate[TypeConverter](
    "orm.typeConverter", DefaultTypeConverter)

  /**
   * The schema name which is used if not specified explicitly.
   * Can be overriden with `orm.defaultSchema` configuration parameter.
   */
  val defaultSchema = cx.get("orm.defaultSchema") match {
    case Some(s: String) => new Schema(s)
    case _ => new Schema("public")
  }

  /**
   * Transaction manager.
   * Can be overriden with `orm.transactionManager` configuration parameter.
   */
  val transactionManager: TransactionManager = cx.instantiate[TransactionManager](
    "orm.transactionManager", new ContextTransactionManager)

  /**
   * Thread local to hold temporary aliases.
   *
   * This is necessary to propagate an alias from `RelationNode` to `Field` with
   * the help of implicits.
   */
  private val _lastAlias = new ThreadLocal[String]

  def lastAlias: Option[String] =
    if (_lastAlias.get == null) None
    else {
      val a = _lastAlias.get
      _lastAlias.set(null)
      Some(a)
    }
  def lastAlias(alias: String): Unit = _lastAlias.set(alias)

}

// ### Connection provider

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
      ormLog.info("Using READ COMMITTED isolation, override 'orm.connection.isolation' if necesssary.")
      Connection.TRANSACTION_READ_COMMITTED
    }
  }

  /**
   * Configure datasource instance. It is retrieved from JNDI if 'orm.connection.datasource'
   * is specified or is constructed using c3p0 otherwise.
   */
  protected val ds: DataSource = cx.get("orm.connection.datasource") match {
    case Some(jndiName: String) => {
      val ctx = new InitialContext
      val ds = ctx.lookup(jndiName).asInstanceOf[DataSource]
      ormLog.info("Using JNDI datasource ({}).", jndiName)
      ds
    }
    case _ => {
      ormLog.info("Using c3p0 connection pooling.")
      val driver = cx.get("orm.connection.driver") match {
        case Some(s: String) => s
        case _ =>
          throw new ORMException("Missing mandatory configuration parameter 'orm.connection.driver'.")
      }
      val url = cx.get("orm.connection.url") match {
        case Some(s: String) => s
        case _ =>
          throw new ORMException("Missing mandatory configuration parameter 'orm.connection.url'.")
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

  def dataSource: DataSource = ds

  /**
   * Open a new JDBC connection.
   */
  def openConnection: Connection = {
    val conn = dataSource.getConnection
    conn.setAutoCommit(autocommit)
    conn.setTransactionIsolation(isolation)
    return conn
  }

}

object DefaultConnectionProvider extends DefaultConnectionProvider

// ### Type converter

/**
 * *Type converters* are used to read atomic values from JDBC result sets and to set
 * JDBC prepared statement values for execution. If you intend to use custom types,
 * provide your own implementation.
 */
trait TypeConverter {

  /**
   * Read a value from specified `ResultSet` at specified column `alias`.
   */
  def read(rs: ResultSet, alias: String): Any = {
    val result = rs.getObject(alias)
    if (rs.wasNull) return null
    else return result
  }

  /**
   * Write a value to specified `PreparedStatement` at specified `paramIndex`.
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
   * Convert a value to string and return it with SQL-compliant escaping.
   */
  def escape(value: Any): String = convert(value) match {
    case None | null => "null"
    case s: String => dialect.quoteLiteral(s)
    case d: Timestamp => dialect.quoteLiteral(d.toString)
    case other => other.toString
  }
}

object DefaultTypeConverter extends TypeConverter
