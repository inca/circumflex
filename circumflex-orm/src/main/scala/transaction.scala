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

import java.sql.Connection

/**
 * Defines a contract to open stateful transactions and return thread-locally current transaction.
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
   * Closes up the transaction and any associated resources.
   */
  def close(): Unit =
    if (!live_?) return
    else {
      // clean up
      connection.close
    }

}
