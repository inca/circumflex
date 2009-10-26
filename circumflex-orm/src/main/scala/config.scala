package ru.circumflex.orm

import com.mchange.v2.c3p0.ComboPooledDataSource
import core.{RouteContext, AbstractCircumflexFilter}
import javax.servlet.FilterChain
import java.sql.Connection
import java.util.{MissingResourceException, ResourceBundle}
import javax.naming.InitialContext
import javax.sql.DataSource
import org.slf4j.LoggerFactory

/**
 * Defines a contract to return JDBC connections.
 */
trait ConnectionProvider {
  /**
   * Returns a JDBC connection.
   * @return JDBC connection
   */
  def getConnection: Connection

}

/**
 * Implements CurrentConnectionProvider with connection-per-thread pattern.
 * Uses <code>java.sql.Connection.isClosed</code> to identify used connections.
 */
trait ThreadLocalConnectionProvider extends ConnectionProvider {
  private val threadLocalContext = new ThreadLocal[Connection]

  /**
   * Opens new JDBC connection.
   * @return new JDBC connection
   */
  def openConnection: Connection

  /**
   * Returns true if connection is bound to thread local and not yet closed.
   */
  def hasLiveConnection: Boolean =
    threadLocalContext.get != null && !threadLocalContext.get.isClosed

  /**
   * Returns live JDBC connection or opens a new one and binds it to thread local.
   * @return JDBC connection
   */
  def getConnection: Connection = {
    if (!hasLiveConnection) threadLocalContext.set(openConnection)
    return threadLocalContext.get
  }
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
 *    one of <code>none</code>, <code>read_uncommited</code>, <code>read_commited</code>,
 *    <code>repeatable_read</code>, <code>serializable</code>; defaults to <code>read_commited</code>).</dd>
 * </dl>
 * See <a href="http://www.mchange.com/projects/c3p0/index.html#configuration_properties">c3p0 configuration
 * properties reference</a> for information regarding connection pool configuration.
 */
class DefaultConnectionProvider extends ThreadLocalConnectionProvider {

  protected val log = LoggerFactory.getLogger("ru.circumflex.orm")

  /**
   * Returns a configuration resource bundle.
   */
  def configurationBundle = ResourceBundle.getBundle("cx")

  private val driver = configurationBundle.getString("orm.connection.driver")
  private val url = configurationBundle.getString("orm.connection.url")
  private val username = configurationBundle.getString("orm.connection.username")
  private val password = configurationBundle.getString("orm.connection.password")
  private val isolation: Int = try {
    configurationBundle.getString("orm.connection.isolation") match {
      case "none" => Connection.TRANSACTION_NONE
      case "read_uncommited" => Connection.TRANSACTION_READ_UNCOMMITTED
      case "read_commited" => Connection.TRANSACTION_READ_COMMITTED
      case "repeatable_read" => Connection.TRANSACTION_REPEATABLE_READ
      case "serializable" => Connection.TRANSACTION_SERIALIZABLE
    }
  } catch {
    case e: MissingResourceException => Connection.TRANSACTION_READ_COMMITTED
    case e => throw e
  }

  /**
   * Retrieves a datasource from JNDI or, if failed, constructs a pooled datasource using c3p0.
   */
  def dataSource: DataSource = try {
    val jndiName = configurationBundle.getString("orm.connection.datasource")
    val ctx = new InitialContext
    val ds = ctx.lookup(jndiName).asInstanceOf[DataSource]
    log.info("Using JNDI datasource ({}).", jndiName)
    return ds
  } catch {
    case _ => {
      log.info("Using c3p0 connection pooling.")
      val ds = new ComboPooledDataSource()
      ds.setDriverClass(driver)
      ds.setJdbcUrl(url)
      ds.setUser(username)
      ds.setPassword(password)
      return ds
    }
  }

  private val ds = dataSource

  def openConnection: Connection = {
    val conn = ds.getConnection
    conn.setTransactionIsolation(isolation)
    return conn
  }

}

object DefaultConnectionProvider extends DefaultConnectionProvider

/**
 * Ensures that current transaction is commited and that contextual connection is closed
 * at the end of request processing cycle.
 * This filter should be the first in chain.
 */
class ORMFilter extends AbstractCircumflexFilter {
  protected val log = LoggerFactory.getLogger("ru.circumflex.orm.filter")
  /**
   * Commits current transaction at the end of request processing cycle and closes current connection.
   */
  def doFilter(ctx: RouteContext, chain: FilterChain) = {
    chain.doFilter(ctx.request, ctx.response)
    if (DefaultConnectionProvider.hasLiveConnection) try {
      DefaultConnectionProvider.getConnection.commit
      log.trace("Committed current transaction.")
    } catch {
      case e => {
        log.error("An error has occured while trying to commit current transaction.", e)
        DefaultConnectionProvider.getConnection.rollback
        log.trace("Rolled back current transaction.")
      }
    } finally {
      DefaultConnectionProvider.getConnection.close
      log.trace("Closed current connection.")
    }
  }
}

/**
 * Aggregates all ORM-related interfaces into configuration object.
 * You may want to provide your own implementation of all these methods if you are not satisfied with
 * default ones.
 */
trait Configuration {
  def connectionProvider: ConnectionProvider
  def dialect: Dialect
}

/**
 * Aggregates default implementations of ORM-related interfaces.
 */
object DefaultConfiguration extends Configuration {

  def connectionProvider = DefaultConnectionProvider

  def dialect = DefaultDialect

}

