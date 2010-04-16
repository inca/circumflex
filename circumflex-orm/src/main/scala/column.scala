package ru.circumflex.orm

import ru.circumflex.core.WrapperModel
import ORM._

/* ## Column */

/**
 * *Column* is an atomic data carrier unit. Each field of persistent class
 * correspond to a column in a table. We strongly distinguish nullable and
 * non-nullable columns.
 */
abstract class Column[T](val sqlType: String) extends WrapperModel {

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
  protected[orm] var _name: Option[String] = None
  def name = _name
  def name(name: String): this.type = {
    _name = Some(name)
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

class NotNullColumn[T](t: String) extends Column[T](t) {
  def :=(newValue: T): this.type = {
    setValue(newValue)
    return this
  }
  def nullable(): NullableColumn[T] = {
    val c = new NullableColumn[T](sqlType)
    c._default = this.default
    c._name = this.name
    return c
  }
}

class NullableColumn[T](t: String) extends Column[Option[T]](t) {
  def get(): T = _value.get
  def getOrElse(default: T): T = apply().getOrElse(default)
  def :=(newValue: T): this.type = {
    setValue(Some(newValue))
    return this
  }
  def null_!() = setValue(None)
  def notNull(): NotNullColumn[T] = {
    val c = new NotNullColumn[T](sqlType)
    c._default = this.default
    c._name = this.name
    return c
  }
  override def toString = apply() match {
    case Some(value) if value != null => value.toString
    case _ => ""
  }
}

/* ## Meta for columns */

class ColumnMeta[T](val column: Column[T],
                    val inferredName: String) {
  val columnName: String = column.name match {
    case Some(n) => n
    case _ => inferredName
  }
  val sqlType = column.sqlType
  val nullable = column.isInstanceOf[NullableColumn[T]]
  val default = column.default
  def sqlDefinition: String = dialect.columnDefinition(this)
}
