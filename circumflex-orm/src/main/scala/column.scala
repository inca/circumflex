package ru.circumflex.orm

import ru.circumflex.core.WrapperModel
import ORM._

/* ## Column */

/**
 * *Column* is an atomic data carrier unit. Each field of persistent class
 * correspond to a column in a table. We strongly distinguish nullable and
 * non-nullable columns.
 */
abstract class Column[T, R <: Relation](val record: R,
                                      val sqlType: String,
                                      val customName: Option[String],
                                      protected var _defaultExpr: Option[String])
        extends WrapperModel {

  // An internally stored value.
  protected var _value: T = _

  // This way the value will be unwrapped by FTL engine.
  def item = getValue

  // Accessors and mutators.

  def getValue(): T = _value
  def setValue(newValue: T) = {_value = newValue}

  def apply(): T = getValue

  /**
   * If provided, overrides the column name obtained via reflection.
   */
  def defaultExpression: Option[String] = _defaultExpr

  def default(expr: String): this.type = {
    _defaultExpr = Some(expr)
    this
  }

  /**
   * Return a `String` representation of internal value.
   */
  override def toString = if (getValue == null) "" else getValue.toString

}

class NotNullColumn[T, R <: Relation](r: R,
                                    t: String,
                                    n: Option[String],
                                    d: Option[String])
        extends Column[T, R](r, t, n, d) {
  def :=(newValue: T) = setValue(newValue)
  def nullable(): NullableColumn[T, R] =
    new NullableColumn[T, R](record, sqlType, customName, defaultExpression)
}

class NullableColumn[T, R <: Relation](r: R,
                                     t: String,
                                     n: Option[String],
                                     d: Option[String])
        extends Column[Option[T], R](r, t, n, d) {
  def get(): T = _value.get
  def getOrElse(default: T): T = apply().getOrElse(default)
  def :=(newValue: T) = setValue(Some(newValue))
  def null_!() = setValue(None)
  def notNull(): NotNullColumn[T, R] =
    new NotNullColumn[T, R](record, sqlType, customName, defaultExpression)
  override def toString = apply() match {
    case Some(value) if value != null => value.toString
    case _ => ""
  }
}

/* ## Meta for columns */

class ColumnMeta[T, R <: Relation](val column: Column[T, R],
                       val inferredName: String) {
  val columnName: String = column.customName match {
    case Some(n) => n
    case _ => inferredName
  }
  val sqlType = column.sqlType
  def nullable_?() = column.isInstanceOf[NullableColumn[T, R]]
  def default = column.defaultExpression
  def sqlDefinition: String = dialect.columnDefinition(this)
}