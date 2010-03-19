package ru.circumflex.orm

import com.mchange.v2.c3p0.ComboPooledDataSource
import ru.circumflex.core.Circumflex
import java.sql.{Timestamp, PreparedStatement, ResultSet, Connection}
import java.util.Date
import javax.naming.InitialContext
import javax.sql.DataSource
import org.slf4j.LoggerFactory
import ORM._

/**
 * Defines a contract to return stateful transactions.
 */
trait ConnectionProvider {

  /**
   * Opens new JDBC connection.
   * @return new JDBC connection
   */
  def openConnection: Connection

}

/**
 * Default ConnectionProvider implementation, used to acquire connections throughout the application.
 * It reads datasource settings from configuration bundle (<code>cx.orm.properties</code>
 * by default).
 * If <code>connection.datasource</code> is specified, datasource is retrieved using JNDI;
 * otherwise built-in c3p0 connection pooling used.
 * The following properties are expected:
 * <dl>
 * <dt>orm.connection.datasource</dt><dd>Datasource JNDI name</dd>
 * <dt>orm.connection.driver</dt><dd>Fully-qualified class name of JDBC driver class (vendor-specific)</dd>
 * <dt>orm.connection.url</dt><dd>JDBC datasource url</dd>
 * <dt>orm.connection.username</dt><dd>database username</dd>
 * <dt>orm.connection.password</dt><dd>database password</dd>
 * <dt>orm.connection.isolation</dt><dd>optional, isolation level for connections (
 *    one of <code>none</code>, <code>read_uncommitted</code>, <code>read_committed</code>,
 *    <code>repeatable_read</code>, <code>serializable</code>; defaults to <code>read_committed</code>).</dd>
 * </dl>
 * See <a href="http://www.mchange.com/projects/c3p0/index.html#configuration_properties">c3p0 configuration
 * properties reference</a> for information regarding connection pool configuration.
 */
class DefaultConnectionProvider extends ConnectionProvider {
  protected val log = LoggerFactory.getLogger("ru.circumflex.orm")

  protected val isolation: Int = Circumflex.cfg("orm.connection.isolation") match {
    case Some("none") => Connection.TRANSACTION_NONE
    case Some("read_uncommitted") => Connection.TRANSACTION_READ_UNCOMMITTED
    case Some("read_committed") => Connection.TRANSACTION_READ_COMMITTED
    case Some("repeatable_read") => Connection.TRANSACTION_REPEATABLE_READ
    case Some("serializable") => Connection.TRANSACTION_SERIALIZABLE
    case _ => {
      log.info("Using READ COMMITTED isolation, override 'orm.connection.isolation' if necesssary.")
      Connection.TRANSACTION_READ_COMMITTED
    }
  }

  protected val ds: DataSource = Circumflex.cfg("orm.connection.datasource") match {
    case Some(jndiName: String) => {
      val ctx = new InitialContext
      val ds = ctx.lookup(jndiName).asInstanceOf[DataSource]
      log.info("Using JNDI datasource ({}).", jndiName)
      ds
    }
    case _ => {
      log.info("Using c3p0 connection pooling.")
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

  /**
   * Returns configured datasource instance. It is retrieved from JNDI if 'orm.connection.datasource'
   * is specified; or constructs a pooled datasource using c3p0 otherwise.
   */
  def dataSource: DataSource = ds

  /**
   * Opens a new JDBC connection.
   */
  def openConnection: Connection = {
    val conn = dataSource.getConnection
    conn.setAutoCommit(false)
    conn.setTransactionIsolation(isolation)
    return conn
  }

}

object DefaultConnectionProvider extends DefaultConnectionProvider

/**
 * Type converters are used to read atomic values from JDBC
 * result sets and to set JDBC prepared statement values for execution.
 * If you intend to use custom types, provide your own implementation.
 */
trait TypeConverter {

  def read(rs: ResultSet, alias: String): Option[Any] = {
    val result = rs.getObject(alias)
    if (rs.wasNull) return None
    else return Some(result)
  }

  def write(st: PreparedStatement, parameter: Any, paramIndex: Int): Unit = parameter match {
    case Some(p) => write(st, p, paramIndex)
    case None | null => st.setObject(paramIndex, null)
    case value => st.setObject(paramIndex, convert(value))
  }

  def convert(value: Any): Any = value match {
    case (p: Date) => new Timestamp(p.getTime)
    case value => value
  }

  def toString(value: Any): String = convert(value) match {
    case None | null => "null"
    case s: String => dialect.quoteLiteral(s)
    case d: Timestamp => dialect.quoteLiteral(d.toString)
    case other => other.toString
  }
}

object DefaultTypeConverter extends TypeConverter
