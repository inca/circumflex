package ru.circumflex.orm

import ru.circumflex.core._
import javax.sql.DataSource
import javax.naming.InitialContext
import com.mchange.v2.c3p0.ComboPooledDataSource
import java.sql.Connection

/*!# ORM Configuration Objects

Circumflex ORM needs to know a little about your environment to operate.
Following objects configure different aspects of Circumflex ORM:

  * [connection provider](#connection-provider) is used to acquire JDBC
  connections, can be overriden by setting the `orm.connectionProvider`
  configuration parameter;
  
*/
object ORM {

  val connectionProvider: ConnectionProvider = cx.instantiate[ConnectionProvider](
    "orm.connectionProvider", new DefaultConnectionProvider)

}

/*!## Connection Provider

The `ConnectionProvider` is a simple trait responsible for acquiring JDBC
connections throughout the application.
*/
trait ConnectionProvider {
  /**
   * Opens new JDBC connection.
   */
  def openConnection: Connection
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
   * set *auto-commit* for each connection to `false`
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

  /**
   * Configure datasource instance. It is retrieved from JNDI if 'orm.connection.datasource'
   * is specified or is constructed using c3p0 otherwise.
   */
  protected val ds: DataSource = cx.get("orm.connection.datasource") match {
    case Some(jndiName: String) => {
      val ctx = new InitialContext
      val ds = ctx.lookup(jndiName).asInstanceOf[DataSource]
      ORM_LOG.info("Using JNDI datasource ({}).", jndiName)
      ds
    }
    case _ => {
      ORM_LOG.info("Using c3p0 connection pool.")
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