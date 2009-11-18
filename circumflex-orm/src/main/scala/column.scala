package ru.circumflex.orm


import java.sql.{PreparedStatement, ResultSet}


/**
 * Base functionality for columns.
 */
abstract class Column[T](val table: Table,
                         val columnName: String,
                         val sqlType: String)
    extends SchemaObject {

  protected var _nullable = true;
  protected var _sequence: Option[Sequence] = None;

  /**
   * DSL-like way to qualify a column with NOT NULL constraint.
   */
  def notNull: Column[T] = {
    _nullable = false
    return this
  }

  /**
   * Is this column nullable?
   */
  def nullable: Boolean = _nullable

  /**
   * Is this column's value generated using database sequence?
   */
  def generated: Boolean = false

  /**
   * Get a sequence for autoincrement columns.
   */
  def sequence: Option[Sequence] = _sequence

  /**
   * DSL-like way to qualify a column with UNIQUE constraint.
   */
  def unique: Column[T] = {
    table.unique(this)
    return this
  }

  /**
   * DSL-like way to transform a column to foreign key association.
   */
  def references(referenceTable: Table): ForeignKey =
    table.foreignKey(referenceTable, this)

  /* DDL */

  /**
   * Produces SQL definition for a column (e.q. "mycolumn varchar not null unique")
   */
  def sqlDefinition: String = dialect.columnDefinition(this)

  def sqlCreate = dialect.alterTableAddColumn(this)
  def sqlDrop = dialect.alterTableDropColumn(this)

  override def toString = sqlDefinition

  override def equals(obj: Any) = obj match {
    case col: Column[T] =>
      col.table.equals(this.table) &&
          col.columnName.equalsIgnoreCase(this.columnName)
    case _ => false
  }

  override def hashCode = this.table.hashCode * 31 +
      this.columnName.toLowerCase.hashCode
}

/**
 * String column (varchar-typed).
 */
class StringColumn(table: Table, name: String)
    extends Column[String](table, name, table.dialect.stringType)

/**
 * Long column (bigint-typed).
 */
class LongColumn(table: Table, name: String)
    extends Column[Long](table, name, table.dialect.longType) {
  /**
   * DSL-like way to create a sequence for this column.
   */
  def autoIncrement: LongColumn = {
    _sequence = Some(new Sequence(table, this))
    this
  }
}


