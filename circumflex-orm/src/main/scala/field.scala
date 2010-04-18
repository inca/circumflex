package ru.circumflex.orm

import ru.circumflex.core.WrapperModel

/* ## Field */

/**
 * *Field* is an atomic data carrier unit. Each field of persistent class
 * correspond to a field of record in a relation. We strongly distinguish
 * nullable and non-nullable fields.
 */
abstract class Field[R <: Record[R], T](val record: R,
                                        val sqlType: String)
    extends WrapperModel {

  // An internally stored value.
  protected var _value: T = _

  // A default expression for DDL.
  protected[orm] var _default: Option[String] = None
  def default = _default
  def default(expr: String): this.type = {
    _default = Some(expr)
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

class NotNullField[R <: Record[R], T](r: R, t: String) extends Field[R, T](r, t) {
  def :=(newValue: T): this.type = {
    setValue(newValue)
    return this
  }
  def nullable(): NullableField[R, T] = {
    val c = new NullableField[R, T](record, sqlType)
    c._default = this.default
    return c
  }
}

class NullableField[R <: Record[R], T](r: R, t: String) extends Field[R, Option[T]](r, t) {
  def get(): T = _value.get
  def getOrElse(default: T): T = apply().getOrElse(default)
  def :=(newValue: T): this.type = {
    setValue(Some(newValue))
    return this
  }
  def null_!() = setValue(None)
  def notNull(): NotNullField[R, T] = {
    val c = new NotNullField[R, T](record, sqlType)
    c._default = this.default
    return c
  }
  override def toString = apply() match {
    case Some(value) if value != null => value.toString
    case _ => ""
  }
}