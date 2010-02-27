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

import java.sql.{PreparedStatement, Connection}
import collection.mutable.HashMap

/**
 * Defines a contract to open stateful transactions and return
 * thread-locally current transaction.
 */
trait TransactionManager {

  private val threadLocalContext = new ThreadLocal[StatefulTransaction]

  def hasLiveTransaction: Boolean =
    threadLocalContext.get != null && threadLocalContext.get.live_?

  def getTransaction: StatefulTransaction = {
    if (!hasLiveTransaction) threadLocalContext.set(openTransaction)
    return threadLocalContext.get
  }

  def openTransaction(): StatefulTransaction = new StatefulTransaction()

  def sql[A](sql: String)(actions: PreparedStatement => A) =
    getTransaction.sql(sql)(actions)
  
  def dml[A](actions: Connection => A) =
    getTransaction.dml(actions)

}

object DefaultTransactionManager extends TransactionManager


class StatefulTransaction {

  val connection: Connection = ORM.connectionProvider.openConnection
  protected var autoClose = false

  def setAutoClose(value: Boolean): this.type = {
    this.autoClose = value
    return this
  }

  def live_?(): Boolean = connection != null && !connection.isClosed

  def autoClose_?(): Boolean = this.autoClose

  /**
   * Commits the transaction.
   */
  def commit(): Unit = try {
    if (!live_?) return
    connection.commit
  } finally if (autoClose) connection.close

  /**
   * Rolls the transaction back.
   */
  def rollback(): Unit = try {
    if (!live_?) return
    connection.rollback
  } finally if (autoClose) connection.close

  /**
   * Invalidates all caches and clears all state associated with this transaction.
   */
  def cleanup(): this.type = {
    invalidateCaches
    return this
  }

  /**
   * Prepares SQL statement and executes an attached block within the transaction scope.
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
   *  Executes a block with DML-like actions in state-safe manner (does cleanup afterwards).
   */
  def dml[A](actions: Connection => A): A = try {
    actions(connection)
  } finally {
    cleanup()
  }

  /**
   * Closes up the transaction and any associated resources.
   */
  def close(): Unit =
    if (!live_?) return
    else {
      cleanup()
      connection.close
    }

  /* CACHE RELATED STUFF */

  protected var recordCache = initRecordCache
  protected var mtoCache = initMTOCache
  protected var otmCache = initOTMCache

  protected def initRecordCache = new HashMap[Relation[_], HashMap[Any, Any]]() {
    override def get(key: Relation[_]) = super.get(key) match {
      case None =>
        this.update(key, new HashMap[Any, Any]())
        super.get(key)
      case v => v
    }
  }

  protected def initMTOCache = new HashMap[Association[_, _], HashMap[Any, Any]]() {
    override def get(key: Association[_, _]) = super.get(key) match {
      case None =>
        this.update(key, new HashMap[Any, Any]())
        super.get(key)
      case v => v
    }
  }

  protected def initOTMCache = new HashMap[Association[_, _], HashMap[Any, Seq[Any]]]() {
    override def get(key: Association[_, _]) = super.get(key) match {
      case None =>
        this.update(key, new HashMap[Any, Seq[Any]]())
        super.get(key)
      case v => v
    }
  }

  def invalidateCaches: this.type = {
    recordCache = initRecordCache
    mtoCache = initMTOCache
    otmCache = initOTMCache
    return this
  }

  def getCachedRecord[R](relation: Relation[R], id: Any): Option[R] =
    recordCache(relation).get(id) match {
      case Some(null) => None
      case Some(record: R) => Some(record)
      case _ => None
    }

  def updateRecordCache[R](record: Record[R]): this.type = {
    if (!record.identified_?) throw new ORMException("Could not cache unidentified record.")
    recordCache(record.relation) += (record.primaryKey.get -> record)
    return this
  }

  def getCachedMTO[C, P](association: Association[C, P], child: C): Option[P] =
    mtoCache(association).get(child) match {
      case Some(null) => None
      case Some(mto: P) => Some(mto)
      case _ => None
    }

  def updateMTOCache[C, P](association: Association[C, P], child: C, parent: P): this.type = {
    mtoCache(association) += (child -> parent)
    return this
  }

  def evictMTO[C](association: Association[C, _], child: C): this.type = {
    mtoCache(association) -= child
    return this
  }

  def getCachedOTM[C, P](association: Association[C, P], parent: P): Option[Seq[C]] =
    otmCache(association).get(parent) match {
      case Some(otm: Seq[C]) => Some(otm)
      case _ => None
    }

  def updateOTMCache[C, P](association: Association[C, P], parent: P, children: Seq[C]): this.type = {
    otmCache(association) += (parent -> children)
    return this
  }

  def evictOTM[C, P](association: Association[C, P], parent: P): this.type = {
    otmCache(association) -= parent
    return this
  }



}
