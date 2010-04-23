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

  // ### Implicits

  implicit def str2fieldHelper(str: String): FieldHelper[R] = new FieldHelper(this, str)

  // ### Commons

  /**
   * A `Relation[R]` corresponding to this record.
   *
   * In general the relations should be the companion objects of records:
   *
   *     class Country extends Record[Country] {
   *       val name = TEXT NOT_NULL
   *     }
   *
   *     object Country extends Table[Country]
   *
   * However, if you prefer different naming conventions, you should override
   * this method.
   */
  def relation = RelationRegistry.getRelation(this)

  // A default primary key is an auto-incremented `BIGINT` column.
  val id = "id" BIGINT

  /**
   * We only support auto-generated `BIGINT` columns as primary keys
   * for a couple of reasons. Sorry.
   */
  def primaryKey: Field[R, Long] = id

  // ### Miscellaneous

  override def toString = getClass.getSimpleName + "@" + id.toString("UNSAVED")

}

// ## Helper for fields DSL

/**
 * This is a tiny builder that helps to instantiate `Field`s and `Association`s in
 * a neat DSL-like way.
 */
class FieldHelper[R <: Record[R]](record: R, name: String) {

  // ### Fields creation

  def integer = new NotNullField[R, Int](record, name, dialect.integerType)
  def bigint = new NotNullField[R, Long](record, name, dialect.longType)
  def numeric(precision: Int = -1, scale: Int = 0) = {
    val p = if (precision == -1) "" else "(" + precision + "," + scale + ")"
    new NotNullField[R, Long](record, name, dialect.numericType + p)
  }
  def text = new NotNullField[R, String](record, name, dialect.stringType)
  def varchar(length: Int = -1) = {
    val l = if (length == -1) "" else "(" + length + ")"
    new NotNullField[R, String](record, name, dialect.varcharType + l)
  }
  def boolean = new NotNullField[R, Boolean](record, name, dialect.booleanType)
  def date = new NotNullField[R, Date](record, name, dialect.dateType)
  def time = new NotNullField[R, Date](record, name, dialect.timeType)
  def timestamp = new NotNullField[R, Date](record, name, dialect.timestampType)
  def field[T](sqlType: String) = new NotNullField[R, T](record, name, sqlType)

  def INTEGER = integer
  def BIGINT = bigint
  def NUMERIC(precision: Int = -1, scale: Int = 1) = numeric(precision, scale)
  def TEXT = text
  def VARCHAR(length: Int = -1) = varchar(length)
  def BOOLEAN = boolean
  def DATE = date
  def TIME = time
  def TIMESTAMP = timestamp

  // ### Associations creation

  def references[F <: Record[F]](relation: Relation[F]): Assocation[R, F] =
    new NotNullAssociation[R, F](record, name, relation)

  def REFERENCES[F <: Record[F]](relation: Relation[F]): Assocation[R, F] =
    references(relation)
}