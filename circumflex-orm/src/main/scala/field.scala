package ru.circumflex.orm

import ORM._

// ## Field

/**
 * Each field of persistent class correspond to a field of record in a relation.
 * We strongly distinguish `NULLABLE` and `NOT_NULL` fields.
 */
abstract class Field[T](name: String,
                        uuid: String,
                        val sqlType: String)
    extends ValueHolder[T](name, uuid) with SQLable {

  // Should the `UNIQUE` constraint be generated for this field?
  protected var _unique: Boolean = false
  def unique: this.type = {
    _unique = true
    return this
  }
  def UNIQUE: this.type = unique
  def unique_?() = _unique

  // An optional default expression for DDL.
  protected[orm] var _default: Option[String] = None
  def default = _default
  def default(expr: String): this.type = {
    _default = Some(dialect.defaultExpression(expr))
    this
  }
  def DEFAULT(expr: String): this.type = default(expr)

  def toSql = dialect.columnDefinition(this)
}

class NotNullField[T](name: String, uuid: String, sqlType: String)
    extends Field[T](name, uuid, sqlType) {

  def nullable: NullableField[T] = {
    val c = new NullableField[T](this.name, this.uuid, this.sqlType)
    c._default = this.default
    return c
  }
  def NULLABLE = nullable

  def notNull: NotNullField[T] = this
  def NOT_NULL = notNull

}

class NullableField[T](name: String, uuid: String, sqlType: String)
    extends Field[Option[T]](name, uuid, sqlType) {

  // `None` is default value instead of `null`
  setValue(None)

  def get(): T = _value.get
  def getOrElse(default: T): T = apply().getOrElse(default)

  def null_!() = setValue(None)
  def NULL_!() = null_!

  def nullable: NullableField[T] = this
  def NULLABLE = nullable

  def notNull: NotNullField[T] = {
    val c = new NotNullField[T](this.name, this.uuid, this.sqlType)
    c._default = this.default
    return c
  }
  def NOT_NULL = notNull

  override def toString(default: String) = apply() match {
    case Some(value) if value != null => value.toString
    case _ => default
  }
}
