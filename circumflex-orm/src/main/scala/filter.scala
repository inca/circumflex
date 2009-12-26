/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

package ru.circumflex.orm

import com.mchange.v2.c3p0.ComboPooledDataSource
import ru.circumflex.core.{CircumflexContext, AbstractCircumflexFilter, Circumflex}
import java.sql.{Timestamp, PreparedStatement, ResultSet, Connection}
import java.util.Date
import javax.naming.InitialContext
import javax.sql.DataSource
import org.slf4j.LoggerFactory
import ORM._
import javax.servlet.{ServletRequestEvent, ServletRequestListener, FilterChain}

/**
 * Defines a contract to return JDBC connections.
 */
trait ConnectionProvider {
  /**
   * Returns a JDBC connection.
   * @return JDBC connection
   */
  def getConnection: Connection

  /**
   * Used to identify whether a connection returned by <code>getConnection</code> is live.
   */
  def hasLiveConnection: Boolean

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

  protected val isolation: Int = Circumflex.cfg("orm.connection.isolation") match {
    case Some("none") => Connection.TRANSACTION_NONE
    case Some("read_uncommited") => Connection.TRANSACTION_READ_UNCOMMITTED
    case Some("read_commited") => Connection.TRANSACTION_READ_COMMITTED
    case Some("repeatable_read") => Connection.TRANSACTION_REPEATABLE_READ
    case Some("serializable") => Connection.TRANSACTION_SERIALIZABLE
    case _ => {
      log.info("Using READ COMMITED isolation, override 'orm.connection.isolation' if necesssary.")
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
        case _ => throw new ORMException("Missing mandatory configuration parameter 'orm.connection.driver")
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
 * Ensures that current transaction is commited and that contextual connection is closed
 * at the end of request processing cycle.
 * This filter should be the first in chain.
 */
class ConnectionManagementFilter extends AbstractCircumflexFilter {
  override protected val log = LoggerFactory.getLogger("ru.circumflex.orm")

  /**
   * Commits current transaction at the end of request processing cycle and closes current connection.
   */
  def doFilter(ctx: CircumflexContext, chain: FilterChain) = {
    chain.doFilter(ctx.request, ctx.response)
    if (connectionProvider.hasLiveConnection) try {
      connectionProvider.getConnection.commit
      log.debug("Committed current transaction.")
    } catch {
      case e => {
        log.error("An error has occured while trying to commit current transaction.", e)
        connectionProvider.getConnection.rollback
        log.debug("Rolled back current transaction.")
      }
    } finally {
      connectionProvider.getConnection.close
      log.debug("Closed current connection.")
    }
  }
}

class ConnectionManagementListener extends ServletRequestListener {
  protected val log = LoggerFactory.getLogger("ru.circumflex.orm")

  def requestInitialized(sre: ServletRequestEvent) = {}

  def requestDestroyed(sre: ServletRequestEvent) =
    if (connectionProvider.hasLiveConnection) try {
      connectionProvider.getConnection.commit
      log.debug("Committed current transaction.")
    } catch {
      case e => {
        log.error("An error has occured while trying to commit current transaction.", e)
        connectionProvider.getConnection.rollback
        log.debug("Rolled back current transaction.")
      }
    } finally {
      connectionProvider.getConnection.close
      log.debug("Closed current connection.")
    }
}

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
