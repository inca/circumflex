package ru.circumflex.orm

import ru.circumflex.core._
import jdbc._
import java.sql.{PreparedStatement, Connection}

/*!# Transaction management

Datatabase transaction boundaries are always necessary. No communication with the
database can occur outside of a database transaction (this seems to confuse many
developers who are used to the auto-commit mode). Always use clear transaction
boundaries, even for read-only operations. Depending on your isolation level and
database capabilities this might not be required but there is no downside if you
always demarcate transactions explicitly.

*Transaction demarcation* refers to setting the transaction boundaries.
There are several popular transaction demarcation patterns for various application types,
most of which operate with some sort of "context" or "scope", to which a single
transaction corresponds. For example, in web applications a transaction may correspond
to a single request.
*/
class Transaction {

  // Connections are opened lazily
  protected var _connection: Connection = null

  def live_?(): Boolean =
    _connection != null && !_connection.isClosed

  def commit(): Unit =
    if (live_? && !_connection.getAutoCommit) _connection.commit

  def rollback(): Unit =
    if (live_? && !_connection.getAutoCommit) _connection.rollback

  def close(): Unit =
    if (live_?) _connection.close

  protected def getConnection: Connection = {
    if (_connection == null)
      _connection = ORM.connectionProvider.openConnection
    return _connection
  }

  def execute[A](connActions: Connection => A)
                (errActions: Throwable => A): A =
    try {
      connActions(getConnection)
    } catch {
      case e => errActions(e)
    }

  def execute[A](sql: String)
                (stActions: PreparedStatement => A)
                (errActions: Throwable => A): A = execute { conn =>
    ORM_LOG.trace(sql)
    autoClose(conn.prepareStatement(sql))(stActions)(errActions)
  } (errActions)

}


trait TransactionManager {
  def hasLive_?(): Boolean
  def get: Transaction
}

class ContextTransactionManager extends TransactionManager {

  Context.addDestroyListener(c => try {
    get.commit
    ORM_LOG.trace("Committed current transaction.")
  } catch {
    case e =>
      ORM_LOG.error("Could not commit current transaction", e)
      try {
        get.rollback
        ORM_LOG.trace("Rolled back current transaction.")
      } catch {
        case e =>
          ORM_LOG.error("Could not roll back current transaction", e)
      }
  } finally {
    get.close
  })

  def hasLive_?(): Boolean = ctx.contains("orm.transaction")

  def get: Transaction = ctx.get("orm.transaction") match {
    case Some(t: Transaction) => t
    case _ =>
      val t = cx.instantiate[Transaction]("orm.transaction", new Transaction)
      ctx.update("orm.transaction", t)
      return t
  }

}