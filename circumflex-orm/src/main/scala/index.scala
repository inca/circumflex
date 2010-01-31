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

import ORM._

/**
 * Represents SQL index.
 */
class Index[R](val relation: Relation[R],
               val indexName: String) extends SchemaObject {

  private var _expressions: Seq[String] = Nil
  private var _unique: Boolean = false;
  private var _method: String = "btree"
  private var _predicate: Predicate = EmptyPredicate

  /**
   * Returns expressions that define this index.
   */
  def expressions: Seq[String] = _expressions

  /**
   * Adds one or more expressions to index definition.
   */
  def add(expr: String*): this.type = {
    _expressions ++= expr.toList
    return this
  }

  /**
   * Adds one or more columns to index definition.
   */
  def add(col: Column[_, R]): this.type = {
    _expressions ++= List(col.columnName)
    return this
  }

  /**
   * Determines, whether the index is unique or not.
   */
  def unique_?(): Boolean = _unique

  /**
   * DSL-like way to define unique index.
   */
  def unique: this.type = {
    _unique = true
    return this
  }

  /**
   * Returns indexing method name.
   */
  def using: String = _method

  /**
   * DSL-like way to define indexing method.
   */
  def using(method: String): this.type = {
    _method = method
    return this
  }

  /**
   * Returns an indexing predicate.
   */
  def where = _predicate

  /**
   * DSL-like way to define indexing predicate.
   */
  def where(predicate: Predicate): this.type = {
    _predicate = predicate
    return this
  }

  def objectName = indexName

  def sqlCreate = dialect.createIndex(this)

  def sqlDrop = dialect.dropIndex(this)

}