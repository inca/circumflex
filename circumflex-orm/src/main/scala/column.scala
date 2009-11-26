package ru.circumflex.orm

/**
 * Base functionality for columns.
 */
abstract class Column[T, R](val table: Table[R],
                            val columnName: String,
                            val sqlType: String)
    extends SchemaObject {
  protected var _nullable = true;
  protected var _sequence: Option[Sequence[R]] = None;

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
   * Get a sequence for autoincrement columns.
   */
  def sequence: Option[Sequence[R]] = _sequence

  /**
   * DSL-like way to qualify a column with UNIQUE constraint.
   */
  def unique: Column[T, R] = {
    table.unique(this)
    return this
  }

  /**
   * DSL-like way to transform a column to foreign key association.
   */
  def references[P](referenceTable: Table[P]): ForeignKey[T, R, P] =
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
    case col: Column[T, R] =>
      col.table.equals(this.table) &&
          col.columnName.equalsIgnoreCase(this.columnName)
    case _ => false
  }

  override def hashCode = this.table.hashCode * 31 +
      this.columnName.toLowerCase.hashCode
}

/**
 * String (varchar) column.
 */
class StringColumn[R](table: Table[R], name: String)
    extends Column[String, R](table, name, table.dialect.stringType)

/**
 * Long (int8) column.
 */
class LongColumn[R](table: Table[R], name: String)
    extends Column[Long, R](table, name, table.dialect.longType) {
  /**
   * DSL-like way to create a sequence for this column.
   */
  def autoIncrement: LongColumn[R] = {
    _sequence = Some(new Sequence(table, this))
    this
  }
}

/**
 * Boolean column.
 */
class BooleanColumn[R](table: Table[R], name: String)
    extends Column[Boolean, R](table, name, table.dialect.booleanType)


