package ru.circumflex.orm

import ru.circumflex.core.WrapperModel
import ORM._

/* ## Column */

/**
 * *Column* is an atomic data carrier unit. Each field of persistent class
 * correspond to a column in a table. We strongly distinguish nullable and
 * non-nullable columns.
 */
abstract class Column[R <: Relation[R], T](val relation: R,
                                           val sqlType: String) extends WrapperModel {

  // An internally stored value.
  protected var _value: T = _

  // A default expression for DDL.
  protected[orm] var _default: Option[String] = None
  def default = _default
  def default(expr: String): this.type = {
    _default = Some(expr)
    this
  }

  // A custom name overrides the one inferred via reflection.
  protected[orm] var _columnName: Option[String] = None
  def columnName = _columnName
  def columnName(columnName: String): this.type = {
    _columnName = Some(columnName)
    this
  }

  // This way the value will be unwrapped by FTL engine.
  def item = getValue

  // Accessors and mutators.

  def getValue(): T = _value
  def setValue(newValue: T) = {_value = newValue}

  def apply(): T = getValue

  /**
   * Return a `String` representation of internal value.
   */
  override def toString = if (getValue == null) "" else getValue.toString

}

class NotNullColumn[R <: Relation[R], T](r: R, t: String) extends Column[R, T](r, t) {
  def :=(newValue: T): this.type = {
    setValue(newValue)
    return this
  }
  def nullable(): NullableColumn[R, T] = {
    val c = new NullableColumn[R, T](relation, sqlType)
    c._default = this.default
    c._columnName = this.columnName
    return c
  }
}

class NullableColumn[R <: Relation[R], T](r: R, t: String) extends Column[R, Option[T]](r, t) {
  def get(): T = _value.get
  def getOrElse(default: T): T = apply().getOrElse(default)
  def :=(newValue: T): this.type = {
    setValue(Some(newValue))
    return this
  }
  def null_!() = setValue(None)
  def notNull(): NotNullColumn[R, T] = {
    val c = new NotNullColumn[R, T](relation, sqlType)
    c._default = this.default
    c._columnName = this.columnName
    return c
  }
  override def toString = apply() match {
    case Some(value) if value != null => value.toString
    case _ => ""
  }
}