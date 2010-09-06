package ru.circumflex.orm

import ru.circumflex.core._
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
  protected def init(): Unit = if (_connection == null)
    _connection = ORM.connectionProvider.openConnection

  def live_?(): Boolean = _connection != null && !_connection.isClosed
  def commit(): Unit = if (live_?) _connection.commit
  def rollback(): Unit = if (live_?) _connection.rollback
  def close(): Unit = if (live_?) _connection.close

  def execute[A](sql: String)(actions: PreparedStatement => A): A = {
    init()
    val st = _connection.prepareStatement(sql)
    try {
      return actions(st)
    } finally {
      st.close
    }
  }
  def execute[A](actions: Connection => A): A = {
    init()
    actions(connection)
  }
}

trait TransactionManager {
  def get: Transaction
  def execute[A](block: => A): A
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

  def get: Transaction = ctx.get("orm.transaction") match {
    case Some(t: Transaction) => t
    case _ =>
      val t = cx.instantiate[Transaction]("orm.transaction", new Transaction)
      ctx.update("orm.transaction", t)
      return t
  }
  def execute[A](block: A): A = {

  }
}


// ### Stateful Transactions

/**
 * The point to use extra-layer above standard JDBC connections is to maintain
 * a cache for each transaction.
 */
class StatefulTransaction {

  /**
   * Undelying JDBC connection.
   */
  val connection: Connection = ORM.connectionProvider.openConnection

  /**
   * Should underlying connection be closed on `commit` or `rollback`?
   */
  protected var autoClose = false

  def setAutoClose(value: Boolean): this.type = {
    this.autoClose = value
    return this
  }

  def autoClose_?(): Boolean = this.autoClose

  /**
   * Is underlying connection alive?
   */
  def live_?(): Boolean = connection != null && !connection.isClosed

  /**
   * Commit the transaction (and close underlying connection if `autoClose` is set to `true`).
   */
  def commit(): Unit = try {
    if (!live_?) return
    connection.commit
  } finally {
    if (autoClose) close()
  }

  /**
   * Rollback the transaction (and close underlying connection if `autoClose` is set to `true`).
   */
  def rollback(): Unit = try {
    if (!live_?) return
    connection.rollback
  } finally {
    if (autoClose) connection.close
  }

  /**
   * Close the underlying connection and dispose of any resources associated with this
   * transaction.
   */
  def close(): Unit =
    if (!live_?) return
    else connection.close()


}
