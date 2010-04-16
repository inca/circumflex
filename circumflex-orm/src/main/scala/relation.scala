package ru.circumflex.orm

import ORM._
import java.util.Date

/* ## Relation */

/**
 * *Relation* is a cornerstone of relational model. In general, each instance of
 * pesistent class is stored as a single record in a relation corresponding to that
 * class. Since Circumflex ORM employs the Active Relation design approach to
 * persistence, each persistent class should subclass `Relation`.
 */
class Relation[R <: Relation[R]] { this: R =>

  // If provided, overrides the name inferred via reflection.
  def relationName: Option[String] = None

  // A default primary key is auto-incremented `BIGINT` column.
  def primaryKey = longColumn.columnName("id").autoIncrement

  /* ### Column creation */
  def intColumn() = new NotNullColumn[Integer](dialect.integerType)
  def longColumn() = new NotNullColumn[Long](dialect.longType)
  def numericColumn() = new NotNullColumn[Double](dialect.numericType)
  def numericColumn(precision: Int, scale: Int) =
    new NotNullColumn[Long](dialect.numericType + "(" + precision + "," + scale + ")")
  def stringColumn() = new NotNullColumn[String](dialect.stringType)
  def varcharColumn() = new NotNullColumn[String](dialect.varcharType)
  def varcharColumn(length: Int) =
    new NotNullColumn[String](dialect.varcharType + "(" + length + ")")
  def booleanColumn() = new NotNullColumn[Boolean](dialect.booleanType)
  def dateColumn() = new NotNullColumn[Date](dialect.dateType)
  def timeColumn() = new NotNullColumn[Date](dialect.timeType)
  def timestampColumn() = new NotNullColumn[Date](dialect.timestampType)
  def column[T](sqlType: String) = new NotNullColumn[T](sqlType)

}