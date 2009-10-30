package ru.circumflex.orm

/**
 * Represents an abstract backend-specific column.
 */
abstract class Column[T, R <: Record](val table: Table[R],
                                      val columnName: String,
                                      val sqlType: String) {

  private var nullable = true;

  /**
   * DSL-like way to qualify a column with NOT NULL constraint.
   */
  def notNull: Column[T, R] = {
    this.nullable = false
    return this
  }

  def isNullable: Boolean = nullable

  /**
   * DSL-like way to qualify a column with UNIQUE constraint.
   */
  def unique: Column[T, R] = {
    //TODO add unique constraint to table
    return this
  }

  /**
   * Produces SQL definition for a column (e.q. "mycolumn varchar not null unique")
   * for schema generation purposes using Dialect object provided by
   * table's configuration.
   * @return SQL column definition
   */
  def sqlDefinition: String = table.dialect.columnDefinition(this)

  /**
   * Instantiates a field instance for the record.
   * Implementation should return column-specific field instance.
   * @return Field instance for this column
   */
  def apply(): Field[T, R] = new Field[T, R](this)

}

class StringColumn[R <: Record](table: Table[R], name: String) extends Column[String, R](table, name, table.dialect.stringType)

class LongColumn[R <: Record](table: Table[R], name: String) extends Column[Long, R](table, name, table.dialect.longType)


