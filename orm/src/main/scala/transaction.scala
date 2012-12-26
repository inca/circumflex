package pro.savant.circumflex
package orm

import core._, cache._
import collection.mutable.HashMap
import java.sql.{Connection, PreparedStatement}
import util.control.ControlThrowable

/*!# Transaction manager

No communication with the database can occur outside of a database transaction.

The `Transaction` class wraps JDBC `Connection` and provides simple methods
for committing or rolling back underlying transaction as well as for executing
various typical JDBC statements.

The `TransactionManager` trait is responsible for allocation current transaction
for application's execution context. The default implementation uses `Context`,
however, your application may require different approaches to transaction
demarcation -- in this case you may provide your own implementation.

JDBC `PreparedStatement` objects are also cached within `Transaction` for
performance considerations.
*/
class Transaction {

  // Connections are opened lazily
  protected var _connection: Connection = null

  // Statements are cached by actual SQL
  protected val _statementsCache = new HashMap[String, PreparedStatement]()

  def isLive: Boolean =
    _connection != null && !_connection.isClosed

  def commit() {
    if (isLive && !_connection.getAutoCommit) _connection.commit()
  }

  def rollback() {
    if (isLive && !_connection.getAutoCommit) _connection.rollback()
    cache.invalidateAll()
  }

  def close() {
    if (isLive) try {
      // close all cached statements
      _statementsCache.values.foreach(_.close())
    } finally {
      // clear statements cache
      _statementsCache.clear()
      // close connection
      _connection.close()
      ormConf.statisticsManager.connectionsClosed.incrementAndGet()
      ORM_LOG.trace("Closed a JDBC connection.")
    }
  }

  protected def getConnection: Connection = {
    if (_connection == null || _connection.isClosed) {
      _connection = ormConf.connectionProvider.openConnection()
      ormConf.statisticsManager.connectionsOpened.incrementAndGet()
      ORM_LOG.trace("Opened a JDBC connection.")
    }
    _connection
  }

  // Cache service

  object cache extends HashMap[String, Cache[_]] {

    def forRelation[PK, R <: Record[PK, R]](rel: Relation[PK, R]): Cache[R] = {
      val _key = "RELATION:" + rel.cacheName
      get(_key) match {
        case Some(cache: Cache[R]) => cache
        case _ =>
          val cache = new HashCache[R] with NoLock[R]
          update(_key, cache)
          cache
      }
    }

    def forAssociation[K, C <: Record[_, C], P <: Record[K, P]]
    (association: Association[K, C, P]): Cache[InverseSeq[C]] = {
      val _key = "ASSOCIATION:" + association.cacheName
      get(_key) match {
        case Some(cache: Cache[InverseSeq[C]]) => cache
        case _ =>
          val cache = new HashCache[InverseSeq[C]] with NoLock[InverseSeq[C]]
          update(_key, cache)
          cache
      }
    }

    def invalidateAll() {
      values.foreach(_.invalidate())
    }

  }

  // Execution methods

  def execute[A](connActions: Connection => A,
                 errActions: Throwable => A): A =
    try {
      ormConf.statisticsManager.executions.incrementAndGet()
      val result = connActions(getConnection)
      ormConf.statisticsManager.executionsSucceeded.incrementAndGet()
      result
    } catch {
      case e: Exception =>
        ormConf.statisticsManager.executionsFailed.incrementAndGet()
        errActions(e)
    }

  def execute[A](sql: String,
                 stActions: PreparedStatement => A,
                 errActions: Throwable => A): A = execute({ conn =>
    ORM_LOG.debug(ormConf.prefix(": ")  + sql)
    val st =_statementsCache.get(sql).getOrElse {
      val statement = ormConf.dialect.prepareStatement(conn, sql)
      _statementsCache.update(sql, statement)
      statement
    }
    stActions(st)
  }, errActions)

  def apply[A](block: => A): A = {
    val sp = getConnection.setSavepoint()
    try {
      block
    } catch {
      case e: ControlThrowable =>
        ORM_LOG.trace("Escaped nested transaction via ControlThrowable, ROLLBACK is suppressed.")
        throw e
      case e: Exception =>
        getConnection.rollback(sp)
        throw e
    } finally {
      getConnection.releaseSavepoint(sp)
    }
  }

}

trait TransactionManager {
  def get: Transaction
}

class DefaultTransactionManager extends TransactionManager {

  Context.addDestroyListener(c => try {
    get.commit()
    ORM_LOG.trace("Committed current transaction.")
  } catch {
    case e1: Exception =>
      ORM_LOG.error("Could not commit current transaction", e1)
      try {
        get.rollback()
        ORM_LOG.trace("Rolled back current transaction.")
      } catch {
        case e2: Exception =>
          ORM_LOG.error("Could not roll back current transaction", e2)
      }
  } finally {
    get.close()
  })

  def get: Transaction = ctx.get("orm.transaction") match {
    case Some(t: Transaction) if (t.isLive) => t
    case _ =>
      val t = cx.instantiate[Transaction]("orm.transaction", new Transaction)
      ctx.update("orm.transaction", t)
      t
  }

}

// Special helper for single-user REPL usage
class ConsoleTransactionManager extends TransactionManager {
  var currentTransaction = new Transaction
  def get = currentTransaction
}
