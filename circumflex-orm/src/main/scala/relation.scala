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
abstract class Relation[R <: Relation[R]] { this: R =>

  /* ### Commons */

  // If provided, overrides the name inferred via reflection.
  def relationName: Option[String] = None

  // A default primary key is auto-incremented `BIGINT` column.
  def primaryKey = longColumn.columnName("id")

  // Are DML statements allowed with this relation?
  def readOnly_?(): Boolean = false

  /**
   * Alias is used for all querying stuff. Due to the fact that two relations should not have the
   * same alias in one query there is some magic regarding `"this"` alias: in every query it is
   * expanded with query-unique identifier (e.g. `this_17`).
   */
  protected var _alias = "this"
  def alias = _alias
  def as(a: String): this.type = {
    _alias = a
    this
  }

  /* ### Column creation */
  def intColumn = new NotNullColumn[R, Integer](this, dialect.integerType)
  def longColumn = new NotNullColumn[R, Long](this, dialect.longType)
  def numericColumn = new NotNullColumn[R, Double](this, dialect.numericType)
  def numericColumn(precision: Int, scale: Int) =
    new NotNullColumn[R, Long](this, dialect.numericType + "(" + precision + "," + scale + ")")
  def stringColumn = new NotNullColumn[R, String](this, dialect.stringType)
  def varcharColumn = new NotNullColumn[R, String](this, dialect.varcharType)
  def varcharColumn(length: Int) =
    new NotNullColumn[R, String](this, dialect.varcharType + "(" + length + ")")
  def booleanColumn = new NotNullColumn[R, Boolean](this, dialect.booleanType)
  def dateColumn = new NotNullColumn[R, Date](this, dialect.dateType)
  def timeColumn = new NotNullColumn[R, Date](this, dialect.timeType)
  def timestampColumn = new NotNullColumn[R, Date](this, dialect.timestampType)
  def column[T](sqlType: String) = new NotNullColumn[R, T](this, sqlType)

}