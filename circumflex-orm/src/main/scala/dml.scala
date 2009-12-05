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

import collection.mutable.ListBuffer
import java.sql.PreparedStatement
import Query._

/**
 * Contains functionality for INSERT-SELECT operations.
 * The query must be prepared first with projections matching target relation's
 * columns.
 */
class InsertSelect[R](val relation: Relation[R], val query: Select)
    extends Configurable with JDBCHelper with SQLable {

  /**
   * Executes a query.
   */
  def executeUpdate: Int = {
    val conn = relation.connectionProvider.getConnection
    val sql = toSql
    sqlLog.debug(sql)
    auto(conn.prepareStatement(sql))(st => {
      query.setParams(st, 1)
      return st.executeUpdate
    })
  }

  def toSql: String = dialect.insertSelect(this)
}

/**
 * Contains functionality for DELETE operations. 
 */
class Delete[R](val relation: Relation[R])
    extends JDBCHelper with SQLable{

  private var _predicate: Predicate = EmptyPredicate

  /**
   * Sets WHERE clause of this query.
   */
  def where(predicate: Predicate): this.type = {
    this._predicate = predicate
    return this
  }

  /**
   * Returns the WHERE clause of this query.
   */
  def where: Predicate = this._predicate

  /**
   * Executes a query.
   */
  def executeUpdate: Int = {
    val conn = relation.connectionProvider.getConnection
    val sql = toSql
    sqlLog.debug(sql)
    auto(conn.prepareStatement(sql))(st => {
      setParams(st, 1)
      return st.executeUpdate
    })
  }

  /**
   * Sets prepared statement params of this query starting from specified index.
   * Returns the new starting index of prepared statement.
   */
  def setParams(st: PreparedStatement, startIndex: Int): Int = {
    var paramsCounter = startIndex;
    _predicate.parameters.foreach(p => {
      typeConverter.write(st, p, paramsCounter)
      paramsCounter += 1
    })
    return paramsCounter
  }

  def toSql: String = dialect.delete(this)

}