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
 * Base superclass with functionality for generating SQL constraints.
 */
abstract class Constraint[R](val table: Table[R])
    extends SchemaObject {

  /**
   * Constraint name (should be dialect-specific).
   */
  def constraintName: String

  def sqlCreate: String = dialect.alterTableAddConstraint(this)

  def sqlDrop: String = dialect.alterTableDropConstraint(this)

  /**
   * A "short" constraint definition (constraint-specific part only,
   * e.g. "PRIMARY KEY (id)" or "UNIQUE (name)", dialect-specific).
   */
  def sqlDefinition: String

  /**
   * Full SQL definition statement (should be dialect-specific,
   * e.g. "CONSTRAINT mytable_pkey PRIMARY KEY (id)".
   */
  def sqlFullDefinition = dialect.constraintDefinition(this)

  override def toString = sqlFullDefinition
}

/**
 * Primary key constraint.
 */
class PrimaryKey[T, R](table: Table[R],
                       val column: Column[T, R])
    extends Constraint[R](table) {
  def constraintName = table.dialect.primaryKeyName(this)

  def sqlDefinition = table.dialect.primaryKeyDefinition(this)
}

/**
 * Unique constraint.
 */
class UniqueKey[R](table: Table[R],
                   val columns: Seq[Column[_, R]])
    extends Constraint[R](table) {
  def constraintName = table.dialect.uniqueKeyName(this)

  def sqlDefinition = table.dialect.uniqueKeyDefinition(this)
}

/**
 * Marker interface for foreign key's ON DELETE and ON UPDATE actions.
 */
trait ForeignKeyAction

object NoAction extends ForeignKeyAction
object CascadeAction extends ForeignKeyAction
object RestrictAction extends ForeignKeyAction
object SetNullAction extends ForeignKeyAction
object SetDefaultAction extends ForeignKeyAction

/**
 * Foreign key constraint.
 */
class ForeignKey[T, C, P](table: Table[C],
                          val referenceTable: Table[P],
                          val localColumn: Column[T, C])
    extends Constraint[C](table) with Association[C, P] {
  def parentRelation = referenceTable

  def childRelation = table

  var onDelete: ForeignKeyAction = NoAction
  var onUpdate: ForeignKeyAction = NoAction

  def constraintName = table.dialect.foreignKeyName(this)

  def sqlDefinition = table.dialect.foreignKeyDefinition(this)

  def onDeleteNoAction: ForeignKey[T, C, P] = {
    onDelete = NoAction
    return this
  }

  def onDeleteCascade: ForeignKey[T, C, P] = {
    onDelete = CascadeAction
    return this
  }

  def onDeleteRestrict: ForeignKey[T, C, P] = {
    onDelete = RestrictAction
    return this
  }

  def onDeleteSetNull: ForeignKey[T, C, P] = {
    onDelete = SetNullAction
    return this
  }

  def onDeleteSetDefault: ForeignKey[T, C, P] = {
    onDelete = SetDefaultAction
    return this
  }

  def onUpdateNoAction: ForeignKey[T, C, P] = {
    onUpdate = NoAction
    return this
  }

  def onUpdateCascade: ForeignKey[T, C, P] = {
    onUpdate = CascadeAction
    return this
  }

  def onUpdateRestrict: ForeignKey[T, C, P] = {
    onUpdate = RestrictAction
    return this
  }

  def onUpdateSetNull: ForeignKey[T, C, P] = {
    onUpdate = SetNullAction
    return this
  }

  def onUpdateSetDefault: ForeignKey[T, C, P] = {
    onUpdate = SetDefaultAction
    return this
  }

}
