package ru.circumflex.orm

import ORM._
import java.lang.String
import java.util.Date
import java.sql.ResultSet
import xml.{XML, NodeSeq}

// ## Field

/**
 * Each field of persistent class correspond to a field of record in a relation.
 */
class Field[T](name: String,
               record: Record[_],
               val sqlType: String)
    extends ValueHolder[T](name, record) with SQLable {

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

  def read(rs: ResultSet, alias: String): T = typeConverter.read(rs, alias).asInstanceOf[T]

  def toSql = dialect.columnDefinition(this)
}

class PrimaryKeyField(record: Record[_])
    extends Field[Long]("id", record, dialect.longType) {
  override def default = Some(dialect.primaryKeyExpression(record))
}

abstract class XmlSerializableField[T](name: String, record: Record[_], sqlType: String)
    extends Field[T](name, record, sqlType) with XmlSerializable[T] {
  def to(value: T) = value.toString
}

class IntField(name: String, record: Record[_])
    extends XmlSerializableField[Int](name, record, dialect.integerType) {
  def from(string: String) = string.toInt
}

class LongField(name: String, record: Record[_])
    extends XmlSerializableField[Long](name, record, dialect.longType) {
  def from(string: String) = string.toLong
}

class NumericField(name: String, record: Record[_], precision: Int = -1, scale: Int = 0)
    extends XmlSerializableField[Double](name, record,
      dialect.numericType + (if (precision == -1) "" else "(" + precision + "," + scale + ")")) {
  def from(string: String) = string.toDouble
}

class TextField(name: String, record: Record[_], sqlType: String)
    extends XmlSerializableField[String](name, record, sqlType) {
  def this(name: String, record: Record[_], length: Int = -1) =
    this(name, record, dialect.varcharType + (if (length == -1) "" else "(" + length + ")"))
  def from(string: String) = string
}

class BooleanField(name: String, record: Record[_])
    extends XmlSerializableField[Boolean](name, record, dialect.booleanType) {
  def from(string: String) = string.toBoolean
}

class TimestampField(name: String, record: Record[_])
    extends XmlSerializableField[Date](name, record, dialect.timestampType) {
  def from(string: String) = new Date(java.sql.Timestamp.valueOf(string).getTime)
  override def to(value: Date) = new java.sql.Timestamp(value.getTime).toString
}

class DateField(name: String, record: Record[_])
    extends XmlSerializableField[Date](name, record, dialect.dateType) {
  def from(string: String) = new Date(java.sql.Date.valueOf(string).getTime)
  override def to(value: Date) = new java.sql.Date(value.getTime).toString
}

class TimeField(name: String, record: Record[_])
    extends XmlSerializableField[Date](name, record, dialect.timeType) {
  def from(string: String) = new Date(java.sql.Time.valueOf(string).getTime)
  override def to(value: Date) = new java.sql.Time(value.getTime).toString
}

class XmlField(name: String, record: Record[_])
    extends XmlSerializableField[NodeSeq](name, record, dialect.xmlType) {
  def from(string: String) = XML.loadString(string)
  override def read(rs: ResultSet, alias: String) = from(rs.getString(alias))
}




