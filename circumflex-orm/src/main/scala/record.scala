package ru.circumflex.orm

import ORM._
import java.util.Date

/* ## Record */

/**
 * *Record* is a cornerstone of relational model. In general, each instance of
 * pesistent class is stored as a single record in a relation corresponding to that
 * class. Since Circumflex ORM employs the Active Relation design approach to
 * persistence, each persistent class should subclass `Record`.
 */
abstract class Record[R <: Record[R]] { this: R =>

  /* ### Commons */

  // A default primary key is auto-incremented `BIGINT` column.
  def id = bigint

  /* ### Field creation */
  def integer = new NotNullField[R, Integer](this, dialect.integerType)
  def bigint = new NotNullField[R, Long](this, dialect.longType)
  def numeric = new NotNullField[R, Double](this, dialect.numericType)
  def numeric(precision: Int, scale: Int) =
    new NotNullField[R, Long](this, dialect.numericType + "(" + precision + "," + scale + ")")
  def text = new NotNullField[R, String](this, dialect.stringType)
  def varchar = new NotNullField[R, String](this, dialect.varcharType)
  def varchar(length: Int) =
    new NotNullField[R, String](this, dialect.varcharType + "(" + length + ")")
  def boolean = new NotNullField[R, Boolean](this, dialect.booleanType)
  def date = new NotNullField[R, Date](this, dialect.dateType)
  def time = new NotNullField[R, Date](this, dialect.timeType)
  def timestamp = new NotNullField[R, Date](this, dialect.timestampType)
  def field[T](sqlType: String) = new NotNullField[R, T](this, sqlType)

}