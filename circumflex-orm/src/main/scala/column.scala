package ru.circumflex.orm

import org.slf4j.LoggerFactory
import ORM._

/* ## Column */

/**
 * *Column* is an atomic data carrier unit. Each field of persistent class correspond to
 * a column in a table. We strongly distinguish nullable and non-nullable columns.
 */
class Column[T, R <: Record](val record: R) {
  protected[orm] var _value: T = _
  def this(r: R, v: T) = {
    this(r)
    _value = v
  }
  def apply(): T = _value
  def update(v: T) = { _value = v }

  /**
   * Converts this column into nullable, that is it is now carries `Option[T]` instead
   * of `T` as it's value.
   */
  def nullable(): Column[Option[T], R] = {
    val v = if (_value == null) None else Some(_value)
    new Column[Option[T], R](record, v)
  }
}