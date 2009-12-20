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
 * Base superclass with functionality for generating SQL constraints.
 */
abstract class Constraint[R](val relation: Relation[R],
                             val constraintName: String)
        extends SchemaObject {

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
class PrimaryKey[T, R](relation: Relation[R],
                       constraintName: String,
                       val column: Column[T, R])
    extends Constraint[R](relation, constraintName) {

  def sqlDefinition = dialect.primaryKeyDefinition(this)

}

/**
 * Unique constraint.
 */
class UniqueKey[R](relation: Relation[R],
                   constraintName: String,
                   val columns: Seq[Column[_, R]])
    extends Constraint[R](relation, constraintName) {

  def sqlDefinition = dialect.uniqueKeyDefinition(this)
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
class ForeignKey[T, C, P](val childRelation: Relation[C],
                          val parentRelation: Relation[P],
                          constraintName: String,
                          val childColumn: Column[T, C])
    extends Constraint[C](childRelation, constraintName)
            with Association[C, P] {

  var onDelete: ForeignKeyAction = NoAction
  var onUpdate: ForeignKeyAction = NoAction

  def sqlDefinition = dialect.foreignKeyDefinition(this)

  def onDeleteNoAction: this.type = {
    onDelete = NoAction
    return this
  }

  def onDeleteCascade: this.type = {
    onDelete = CascadeAction
    return this
  }

  def onDeleteRestrict: this.type = {
    onDelete = RestrictAction
    return this
  }

  def onDeleteSetNull: this.type = {
    onDelete = SetNullAction
    return this
  }

  def onDeleteSetDefault: this.type = {
    onDelete = SetDefaultAction
    return this
  }

  def onUpdateNoAction: this.type = {
    onUpdate = NoAction
    return this
  }

  def onUpdateCascade: this.type = {
    onUpdate = CascadeAction
    return this
  }

  def onUpdateRestrict: this.type = {
    onUpdate = RestrictAction
    return this
  }

  def onUpdateSetNull: this.type = {
    onUpdate = SetNullAction
    return this
  }

  def onUpdateSetDefault: this.type = {
    onUpdate = SetDefaultAction
    return this
  }

}

class CheckConstraint[R](relation: Relation[R],
                         constraintName: String,
                         val expression: String)
    extends Constraint[R](relation, constraintName) {

  def sqlDefinition = dialect.checkConstraintDefinition(this)
}
