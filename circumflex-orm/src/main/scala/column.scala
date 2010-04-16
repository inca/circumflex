package ru.circumflex.orm

import ru.circumflex.core.WrapperModel

/* ## Column */

/**
 * *Column* is an atomic data carrier unit. Each field of persistent class
 * correspond to a column in a table. We strongly distinguish nullable and
 * non-nullable columns.
 */
trait Column[T, R <: Record] extends WrapperModel {

  // An internally stored value.
  protected var _value: T = _

  // This way the value will be unwrapped by FTL engine.
  def item = getValue

  // Accessors and mutators.

  def getValue(): T = _value
  def setValue(newValue: T) = {_value = newValue}

  def apply(): T = getValue

  /**
   * An enclosing record.
   */
  def record: R

  /**
   * SQL type to use for DDL generation for this column.
   */
  def sqlType: String

  /**
   * Return a `String` representation of internal value.
   */
  override def toString = if (getValue == null) "" else getValue.toString

}

class NotNullColumn[T, R <: Record](val record: R,
                                    val sqlType: String)
        extends Column[T, R] {
  def :=(newValue: T) = setValue(newValue)
  def nullable(): NullableColumn[T, R] = new NullableColumn[T, R](record, sqlType)
}

class NullableColumn[T, R <: Record](val record: R,
                                     val sqlType: String)
        extends Column[Option[T], R] {
  def get(): T = _value.get
  def getOrElse(default: T): T = apply().getOrElse(default)
  def :=(newValue: T) = setValue(Some(newValue))
  def null_!() = setValue(None)
  def notNull(): NotNullColumn[T, R] = new NotNullColumn[T, R](record, sqlType)
  override def toString = apply() match {
    case Some(value) if value != null => value.toString
    case _ => ""
  }
}
