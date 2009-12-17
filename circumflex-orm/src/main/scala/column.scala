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


import java.util.Date
import ORM._

/**
 * Base functionality for columns.
 */
abstract class Column[T, R](val relation: Relation[R],
                            val columnName: String,
                            val sqlType: String)
    extends SchemaObject {
  protected var _nullable = true;
  protected var _sequence: Option[Sequence[R]] = None;

  def qualifiedName = dialect.qualifyColumn(this)

  /**
   * DSL-like way to qualify a column with NOT NULL constraint.
   */
  def notNull: this.type = {
    _nullable = false
    return this
  }

  /**
   * Is this column nullable?
   */
  def nullable: Boolean = _nullable

  /**
   * Get a sequence for autoincrement columns.
   */
  def sequence: Option[Sequence[R]] = _sequence

  /**
   * DSL-like way to qualify a column with UNIQUE constraint.
   */
  def unique: this.type = {
    relation.unique(this)
    return this
  }

  /**
   * DSL-like way to transform a column to foreign key association.
   */
  def references[P](parentRelation: Relation[P]): ForeignKey[T, R, P] =
    relation.foreignKey(parentRelation, this)

  /* DDL */

  /**
   * Produces SQL definition for a column (e.q. "mycolumn varchar not null unique")
   */
  def sqlDefinition: String = dialect.columnDefinition(this)

  def sqlCreate = dialect.alterTableAddColumn(this)

  def sqlDrop = dialect.alterTableDropColumn(this)

  override def toString = columnName

  override def equals(obj: Any) = obj match {
    case col: Column[T, R] =>
      col.relation.equals(this.relation) &&
          col.columnName.equalsIgnoreCase(this.columnName)
    case _ => false
  }

  override def hashCode = this.relation.hashCode * 31 +
      this.columnName.toLowerCase.hashCode
}

/**
 * String (varchar) column.
 */
class StringColumn[R](relation: Relation[R], name: String)
    extends Column[String, R](relation, name, dialect.stringType) {

  /**
   * DSL-like way to add NotEmptyValidator.
   */
  def validateNotEmpty: this.type = {
    relation.addFieldValidator(this, new NotEmptyValidator(qualifiedName))
    return this
  }

  /**
   * DSL-like way to add PatternValidator.
   */
  def validatePattern(regex: String): this.type = {
    relation.addFieldValidator(this, new PatternValidator(qualifiedName, regex))
    return this
  }

}

/**
 * Long (int8) column.
 */
class LongColumn[R](relation: Relation[R], name: String)
    extends Column[Long, R](relation, name, dialect.longType) {
  /**
   * DSL-like way to create a sequence for this column.
   */
  def autoIncrement: this.type = {
    _sequence = Some(new Sequence(relation, this))
    this
  }
}

/**
 * Boolean column.
 */
class BooleanColumn[R](relation: Relation[R], name: String)
    extends Column[Boolean, R](relation, name, dialect.booleanType)

/**
 * Timestamp column.
 */
class TimestampColumn[R](relation: Relation[R], name: String)
    extends Column[Date, R](relation, name, dialect.timestampType)


