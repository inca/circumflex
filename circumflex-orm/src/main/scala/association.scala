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

/**
 * Defines a contract for parent-child (one-to-many) associations.
 * Usually they are modelled as tables with foreign keys.
 */
trait Association[C, P] {

  import Query._

  /**
   * Returns child relation.
   */
  def childRelation: Relation[C]

  /**
   * Returns parent relation.
   */
  def parentRelation: Relation[P]

  /**
   * Returns a local (a.k.a. child) column.
   */
  def childColumn: Column[_, C]

  /**
   * Alias for childColumn
   */
  def localColumn = childColumn

  /**
   * Returns a referenced (a.k.a. parent) column that matches local column.
   * In normal circumstances this matches parent's primary key.
   */
  def parentColumn: Column[_, P] = parentRelation.primaryKey.column

  /**
   * Alias for parentColumn
   */
  def referenceColumn = parentColumn

  def fetchManyToOne(localValue: Any): Option[P] =
    parentRelation.createCriteria.add(_.projection(parentColumn).eq(localValue)).unique

  def fetchOneToMany(referenceValue: Any): Seq[C] =
    childRelation.createCriteria.add(_.projection(childColumn).eq(referenceValue)).list

}
