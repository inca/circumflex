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

  def qualifiedName = table.tableName + "." + this.columnName

  /**
   * DSL-like way to qualify a column with NOT NULL constraint.
   */
  def notNull: this.type = {
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
  def unique: this.type = {
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
    extends Column[String, R](table, name, table.dialect.stringType) {

  /**
   * DSL-like way to add NotEmptyValidator.
   */
  def validateNotEmpty: this.type = {
    table.addFieldValidator(this, new NotEmptyValidator(qualifiedName))
    return this
  }

  /**
   * DSL-like way to add PatternValidator.
   */
  def validatePattern(regex: String): this.type = {
    table.addFieldValidator(this, new PatternValidator(qualifiedName, regex))
    return this
  }

}

/**
 * Long (int8) column.
 */
class LongColumn[R](table: Table[R], name: String)
    extends Column[Long, R](table, name, table.dialect.longType) {
  /**
   * DSL-like way to create a sequence for this column.
   */
  def autoIncrement: this.type = {
    _sequence = Some(new Sequence(table, this))
    this
  }
}

/**
 * Boolean column.
 */
class BooleanColumn[R](table: Table[R], name: String)
    extends Column[Boolean, R](table, name, table.dialect.booleanType)


