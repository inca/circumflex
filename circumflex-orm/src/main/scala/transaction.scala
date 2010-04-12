package ru.circumflex.orm

import java.sql.{PreparedStatement, Connection}

/* ## Transaction management */

/* ### Transaction demarcation */

// *Transaction demarcation* refers to setting the transaction boundaries.
//
// Datatabase transaction boundaries are always necessary. No communication with the
// database can occur outside of a database transaction (this seems to confuse many
// developers who are used to the auto-commit mode). Always use clear transaction
// boundaries, even for read-only operations. Depending on your isolation level and
// database capabilities this might not be required but there is no downside if you
// always demarcate transactions explicitly.
//
// There are several popular transaction demarcation patterns for various application types,
// most of which operate with some sort of "context" or "scope", to which a single
// transaction corresponds. For example, in web applications a transaction may correspond
// to a single request.

/**
 * ### TransactionManager interface
 *
 * *Transaction manager* aims to help developers demarcate their transactions
 * by providing contextual *current* transaction. By default it uses `ThreadLocal`
 * to bind contextual transactions (a separate transaction is allocated for each thread,
 * and each thread works with one transaction at a given time). You can
 * provide your own transaction manager by implementing the `TransactionManager`
 * trait and setting the `orm.transactionManager` configuration parameter.</p>
 *
 * Defines a contract to open stateful transactions and return
 * thread-locally current transaction.
 */
trait TransactionManager {
  private val threadLocalContext = new ThreadLocal[StatefulTransaction]

  /**
   * Does transaction manager has live current transaction?
   */
  def hasLiveTransaction: Boolean =
    threadLocalContext.get != null && threadLocalContext.get.live_?

  /**
   * Retrieve a contextual transaction.
   */
  def getTransaction: StatefulTransaction = {
    if (!hasLiveTransaction) threadLocalContext.set(openTransaction)
    return threadLocalContext.get
  }

  /**
   * Open new stateful transaction.
   */
  def openTransaction(): StatefulTransaction = new StatefulTransaction()

  /**
   * A shortcut for `getTransaction.sql(sql)(actions)`.
   */
  def sql[A](sql: String)(actions: PreparedStatement => A) =
    getTransaction.sql(sql)(actions)

  /**
   * A shortcut for `getTransaction.dml(actions)`.
   */
  def dml[A](actions: Connection => A) =
    getTransaction.dml(actions)
}

object DefaultTransactionManager extends TransactionManager

/* ### Stateful Transactions */

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
  } finally if (autoClose) connection.close

  /**
   * Rollback the transaction (and close underlying connection if `autoClose` is set to `true`).
   */
  def rollback(): Unit = try {
    if (!live_?) return
    connection.rollback
  } finally if (autoClose) connection.close

  /**
   * Invalidate all caches and clears all state associated with this transaction.
   */
  def cleanup(): this.type = {
    invalidateCaches
    return this
  }

  /**
   * Close the underlying connection and dispose of any resources associated with this
   * transaction.
   */
  def close(): Unit =
    if (!live_?) return
    else {
      cleanup()
      connection.close
    }

  /* ### Database communication methods */

  // In order to ensure that cache is synchronized with transaction we must use these methods
  // to handle all communications with JDBC in a centralized way.
  //
  // The logic is pretty simple: every query that can possibly affect the data
  // in current transaction (i.e. the one that is usually called via `executeUpdate`)
  // should lead to full cache invalidation. This way we must re-read every record
  // after such manipulation -- that fits perfectly with triggers and other stuff that
  // could possibly affect more data at backend than you intended with any particular
  // query.

  /**
   * Prepare SQL statement and execute an attached block within the transaction scope.
   */
  def sql[A](sql: String)(actions: PreparedStatement => A): A = {
    val st = connection.prepareStatement(sql)
    try {
      return actions(st)
    } finally {
      st.close
    }
  }

  /**
   * Execute a block with DML-like actions in state-safe manner (does cleanup afterwards).
   */
  def dml[A](actions: Connection => A): A = try {
    actions(connection)
  } finally {
    cleanup()
  }

  /* ### Cache */

  // TODO

  def invalidateCaches: Unit = {}

}