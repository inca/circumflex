package ru.circumflex.orm

import ORM._
import java.util.Date

// ## Record

/**
 * *Record* is a cornerstone of relational model. In general, each instance of
 * pesistent class is stored as a single record in a relation corresponding to that
 * class. Since Circumflex ORM employs the Active Relation design approach to
 * persistence, each persistent class should subclass `Record`.
 */
abstract class Record[R <: Record[R]] { this: R =>

  // ### Commons

  /**
   * A `Relation[R]` corresponding to this record.
   *
   * In general the relations should be the companion objects of records:
   *
   *     class Country extends Record[Country] {
   *       val name = TEXT.NOT_NULL := "Switzerland"
   *     }
   *
   *     object Country extends Table[Country]
   *
   * However, if you prefer different naming conventions, you should override
   * this method.
   */
  def relation = RelationRegistry.getRelation(this)

  // A default primary key is an auto-incremented `BIGINT` column.
  val id = BIGINT

  /**
   * We only support auto-generated `BIGINT` columns as primary keys
   * for a couple of reasons. Sorry.
   */
  def primaryKey: Field[R, Long] = id

  // ### Fields creation

  def integer = new NotNullField[R, Int](this, dialect.integerType)
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

  def INTEGER = integer
  def BIGINT = bigint
  def NUMERIC = numeric
  def NUMERIC(precision: Int, scale: int) = numeric(precision, scale)
  def TEXT = text
  def VARCHAR = varchar
  def VARCHAR(length: Int) = varchar(length)
  def BOOLEAN = boolean
  def DATE = date
  def TIME = time
  def TIMESTAMP = timestamp

  // ### Associations creation

  def referenceTo[F <: Record[F]](relation: Relation[F]): Assocation[R, F] =
    new NotNullAssociation[R, F](this, relation)

  def REFERENCE_TO[F <: Record[F]](relation: Relation[F]): Assocation[R, F] =
    referenceTo(relation)

  // ### Miscellaneous

  override def toString = getClass.getSimpleName + "@" + id.toString("UNSAVED")

}