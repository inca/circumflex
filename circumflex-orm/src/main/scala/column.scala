package ru.circumflex.orm

/**
 * Represents an abstract backend-specific column.
 */
abstract class Column[T](val table: Table,
                         val columnName: String,
                         val sqlType: String) {

  /**
   * Specifies SQL type of this column.
   * @return column SQL type (e.q. "VARCHAR" or "INT8"); case is ignored.
   */
  def sqlType: String

  private var nullable = true;
  private var unique = false;

  /**
   * DSL-like way to qualify a column with NOT NULL constraint.
   */
  def notNull: Column[T] = {
    this.nullable = false
    return this
  }

  /**
   * DSL-like way to qualify a column with UNIQUE constraint.
   */
  def unique: Column[T] = {
    this.unique = false
    return this
  }

  /**
   * Produces SQL definition for a column (e.q. "mycolumn varchar not null unique")
   * for schema generation purposes using Dialect object provided by
   * table's configuration.
   * @return SQL column definition
   */
  def sqlDefinition: String = table.configuration.dialect.columnDefinition(this)

  /**
   * Instantiates a field instance for the record.
   * Implementation should return column-specific field instance.
   * @return Field instance for this column
   */
  def apply(): Field[T] = new Field[T](this)

}

class StringColumn(table: Table, name: String) extends Column[String](table, name, "varchar")

class BigintColumn(table: Table, name: String) extends Column[Long](table, name, "int8")


