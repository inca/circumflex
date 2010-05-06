package ru.circumflex.orm

import ORM._
import java.lang.String
import java.util.Date

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

abstract class XmlSerializableField[T](name: String, uuid: String, sqlType: String)
    extends Field[T](name, uuid, sqlType) with XmlSerializable[T] {
  def to(value: T) = value.toString
}

class IntField(name: String, uuid: String)
    extends XmlSerializableField[Int](name, uuid, dialect.integerType) {
  def from(string: String) = string.toInt
}

class LongField(name: String, uuid: String)
    extends XmlSerializableField[Long](name, uuid, dialect.longType) {
  def from(string: String) = string.toLong
}

class NumericField(name: String, uuid: String, precision: Int = -1, scale: Int = 0)
    extends XmlSerializableField[Double](
      name,
      uuid,
      dialect.numericType + (if (precision == -1) "" else "(" + precision + "," + scale + ")")) {
  def from(string: String) = string.toDouble
}

class TextField(name: String, uuid: String)
    extends XmlSerializableField[String](name, uuid, dialect.textType) {
  def from(string: String) = string
}

class VarcharField(name: String, uuid: String, length: Int = -1)
    extends XmlSerializableField[String](
      name,
      uuid,
      dialect.varcharType + (if (length == -1) "" else "(" + length + ")")) {
  def from(string: String) = string
}

class BooleanField(name: String, uuid: String)
    extends XmlSerializableField[Boolean](name, uuid, dialect.booleanType) {
  def from(string: String) = string.toBoolean
}

class TimestampField(name: String, uuid: String)
    extends XmlSerializableField[Date](name, uuid, dialect.timestampType) {
  def from(string: String) = new Date(java.sql.Timestamp.valueOf(string).getTime)
  override def to(value: Date) = new java.sql.Timestamp(value.getTime).toString
}

class DateField(name: String, uuid: String)
    extends XmlSerializableField[Date](name, uuid, dialect.dateType) {
  def from(string: String) = new Date(java.sql.Date.valueOf(string).getTime)
  override def to(value: Date) = new java.sql.Date(value.getTime).toString
}

class TimeField(name: String, uuid: String)
    extends XmlSerializableField[Date](name, uuid, dialect.timeType) {
  def from(string: String) = new Date(java.sql.Time.valueOf(string).getTime)
  override def to(value: Date) = new java.sql.Time(value.getTime).toString
}




