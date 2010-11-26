package ru.circumflex.orm

import ru.circumflex.core._
import java.util.Date
import xml._
import java.sql.ResultSet
import scala.math.BigDecimal._

/*!# Field

The `Field` class holds atomic values of records. It wraps actual value
and provides methods for constructing column definitions for enclosing
tables. It also contains the `REFERENCES` method which is used to create
associations.
 */
class Field[T, R <: Record[_, R]](name: String, record: R , sqlType: String)
    extends ValueHolder[T, R](name, record, sqlType) with SQLable {

  def uuid = record.getClass.getName + "." + name

  def read(rs: ResultSet, alias: String): Option[T] = {
    val o = rs.getObject(alias)
    if (rs.wasNull) None
    else Some(o.asInstanceOf[T])
  }

  def REFERENCES[P <: Record[T, P]](relation: Relation[T, P]): Association[T, R, P] =
    new Association(this, relation)

  def toSql: String = dialect.columnDefinition(this)
}

trait AutoIncrementable[T, R <: Record[_, R]] extends Field[T, R] {
  protected var _autoIncrement: Boolean = false
  def autoIncrement_?(): Boolean = _autoIncrement
  def AUTO_INCREMENT(): this.type = {
    _autoIncrement = true
    return this
  }
}

class IntField[R <: Record[_, R]](name: String, record: R)
    extends XmlSerializable[Int, R](name, record, dialect.integerType)
        with AutoIncrementable[Int, R] {
  def from(str: String): Option[Int] =
    try Some(str.toInt) catch { case _ => None }
}

class LongField[R <: Record[_, R]](name: String, record: R)
    extends XmlSerializable[Long, R](name, record, dialect.longType)
        with AutoIncrementable[Long, R]{
  def from(str: String): Option[Long] =
    try Some(str.toLong) catch { case _ => None }
}

class DoubleField[R <: Record[_, R]](name: String,
                                     record: R,
                                     val precision: Int = -1,
                                     val scale: Int = 0)
    extends XmlSerializable[Double, R](name, record, dialect.numericType(precision, scale)) {
  def from(str: String): Option[Double] =
    try Some(str.toDouble) catch { case _ => None }
}

class NumericField[R <: Record[_, R]](name: String,
                                      record: R,
                                      val precision: Int = -1,
                                      val scale: Int = 0,
                                      val roundingMode: RoundingMode.RoundingMode = RoundingMode.HALF_EVEN)
    extends XmlSerializable[BigDecimal, R](name, record, dialect.numericType(precision, scale)) {
  def from(str: String): Option[BigDecimal] =
    try Some(BigDecimal(str)) catch { case _ => None }
  override def read(rs: ResultSet, alias: String): Option[BigDecimal] =
    any2option(rs.getString(alias)).flatMap(x => from(x))
  addSetter(v => v.setScale(scale, roundingMode))
}

class TextField[R <: Record[_, R]](name: String, record: R, sqlType: String)
    extends XmlSerializable[String, R](name, record, sqlType) {
  def this(name: String, record: R, length: Int = -1) =
    this(name, record, dialect.varcharType(length))
  def from(str: String): Option[String] =
    if (str == "") None else Some(str)
}

class BooleanField[R <: Record[_, R]](name: String, record: R)
    extends XmlSerializable[Boolean, R](name, record, dialect.booleanType) {
  def from(str: String): Option[Boolean] =
    try Some(str.toBoolean) catch { case _ => None }
}

class TimestampField[R <: Record[_, R]](name: String, record: R)
    extends XmlSerializable[Date, R](name, record, dialect.timestampType) {
  def from(str: String): Option[Date] =
    try Some(new Date(java.sql.Timestamp.valueOf(str).getTime)) catch { case _ => None }
  override def to(value: Option[Date]): String =
    value.map(v => new java.sql.Timestamp(v.getTime).toString).getOrElse("")
}

class DateField[R <: Record[_, R]](name: String, record: R)
    extends XmlSerializable[Date, R](name, record, dialect.dateType) {
  def from(str: String): Option[Date] =
    try Some(new Date(java.sql.Date.valueOf(str).getTime)) catch { case _ => None }
  override def to(value: Option[Date]): String =
    value.map(v => new java.sql.Date(v.getTime).toString).getOrElse("")
}

class TimeField[R <: Record[_, R]](name: String, record: R)
    extends XmlSerializable[Date, R](name, record, dialect.timeType) {
  def from(str: String): Option[Date] =
    try Some(new Date(java.sql.Time.valueOf(str).getTime)) catch { case _ => None }
  override def to(value: Option[Date]): String =
    value.map(v => new java.sql.Time(v.getTime).toString).getOrElse("")
}

class XmlField[R <: Record[_, R]](name: String, record: R)
    extends XmlSerializable[Elem, R](name, record, dialect.xmlType) {
  def from(str: String): Option[Elem] = Some(XML.loadString(str))
  override def read(rs: ResultSet, alias: String) =
    any2option(rs.getString(alias)).flatMap(x => from(x))
}