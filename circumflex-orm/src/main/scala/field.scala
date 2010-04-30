package ru.circumflex.orm

import ORM._

// ## Field

/**
 * Each field of persistent class correspond to a field of record in a relation.
 */
class Field[T](name: String,
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
  protected[orm] var _defaultExpr: Option[String] = None
  def default = _defaultExpr
  def default(expr: String): this.type = {
    _defaultExpr = Some(dialect.defaultExpression(expr))
    this
  }
  def DEFAULT(expr: String): this.type = default(expr)

  def toSql = dialect.columnDefinition(this)
}

class PrimaryKeyField(val record: Record[_])
    extends Field[Long]("id", record.uuid + "." + "id", dialect.longType) {
  override def default = Some(dialect.primaryKeyExpression(record))
}
