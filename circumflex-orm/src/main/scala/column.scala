package ru.circumflex.orm

/**
 * Base functionality for columns.
 */
abstract class Column[T, R <: Record](val table: Table[R],
                                      val columnName: String,
                                      val sqlType: String)
    extends SchemaObject {

  private var _nullable = true;

  /**
   * DSL-like way to qualify a column with NOT NULL constraint.
   */
  def notNull: Column[T, R] = {
    _nullable = false
    return this
  }

  /**
   * Is this column nullable?
   */
  def nullable: Boolean = _nullable

  /**
   * DSL-like way to qualify a column with UNIQUE constraint.
   */
  def unique: Column[T, R] = {
    table.unique(this)
    return this
  }

  /**
   * DSL-like way to transform a column to association.
   */
  def references[P <: Record](referenceTable: Table[P]): Association[R, P] = {
    val fk = table.foreignKey(referenceTable, this)
    return new Association(fk)
  }

  /**
   * Instantiates a field instance for the record.
   * Implementation should return column-specific field instance.
   * @return Field instance for this column
   */
  def apply(): Field[T, R] = new Field[T, R](this)

  /* DDL */

  /**
   * Produces SQL definition for a column (e.q. "mycolumn varchar not null unique")
   */
  def sqlDefinition: String = dialect.columnDefinition(this)

  def dialect = table.dialect
  def sqlCreate = dialect.alterTableAddColumn(this)
  def sqlDrop = dialect.alterTableDropColumn(this)

}

/**
 * String column (varchar-typed).
 */
class StringColumn[R <: Record](table: Table[R], name: String)
    extends Column[String, R](table, name, table.dialect.stringType)

/**
 * Long column (bigint-typed).
 */
class LongColumn[R <: Record](table: Table[R], name: String)
    extends Column[Long, R](table, name, table.dialect.longType)


