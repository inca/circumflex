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
class Column[T, R](val relation: Relation[R],
                   val columnName: String,
                   val sqlType: String)
    extends SchemaObject {
  protected var _autoIncrement = false
  protected var _nullable = true
  protected var _defaultExpression: Option[String] = None

  def cloneForView[V](view: View[V]): Column[T, V] =
    new Column[T, V](view, relation.relationName + "_" + columnName, sqlType)

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
  def nullable_?(): Boolean = _nullable

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
  def references[P](parentRelation: Relation[P]): AssociativeForeignKey[T, R, P] =
    relation.foreignKey(parentRelation, this)

  /**
   * Returns the default expression for this column.
   */
  def default: Option[String] = _defaultExpression

  /**
   * Sets the default expression for this column.
   */
  def default(expr: String): this.type = {
    _defaultExpression = Some(expr)
    return this
  }

  /**
   * DSL-like way to create a sequence for this column.
   */
  def autoIncrement(): this.type = {
    dialect.prepareAutoIncrementColumn(this)
    _autoIncrement = true
    this
  }

  def autoIncrement_? = _autoIncrement

  /* DDL */

  /**
   * Produces SQL definition for a column (e.q. "mycolumn varchar not null unique")
   */
  def sqlDefinition: String = dialect.columnDefinition(this)

  def sqlCreate = dialect.alterTableAddColumn(this)

  def sqlDrop = dialect.alterTableDropColumn(this)

  def objectName = columnName

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
 * String (text) column.
 */
class StringColumn[R](relation: Relation[R],
                      name: String,
                      sqlType: String)
    extends Column[String, R](relation, name, sqlType) {

  def this(relation: Relation[R], name: String) =
    this(relation, name, dialect.stringType)

  def this(relation: Relation[R], name: String, size: Int) =
    this(relation, name, dialect.varcharType + "(" + size + ")")

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

  /**
   * Sets the default string expression for this column
   * (quoting literals as necessary).
   */
  def defaultString(expr: String): this.type = {
    _defaultExpression = Some(dialect.quoteLiteral(expr))
    return this
  }

}

/**
 * Integer column.
 */
class IntegerColumn[R](relation: Relation[R], name: String)
    extends Column[Int, R](relation, name, dialect.integerType)

/**
 * Long (int8 or bigint) column.
 */
class LongColumn[R](relation: Relation[R], name: String)
    extends Column[Long, R](relation, name, dialect.longType)

/**
 * Integer column.
 */
class NumericColumn[R](relation: Relation[R], name: String, sqlType: String)
    extends Column[Double, R](relation, name, sqlType) {

  def this(relation: Relation[R], name: String, precision: Int, scale: Int) =
    this(relation, name, dialect.numericType + "(" + precision + "," + scale + ")")

  def this(relation: Relation[R], name: String) =
    this(relation, name, dialect.numericType)

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

/**
 * Date column.
 */
class DateColumn[R](relation: Relation[R], name: String)
    extends Column[Date, R](relation, name, dialect.dateType)

/**
 * Time column.
 */
class TimeColumn[R](relation: Relation[R], name: String)
    extends Column[Date, R](relation, name, dialect.timeType)


