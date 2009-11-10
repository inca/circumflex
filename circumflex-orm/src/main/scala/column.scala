package ru.circumflex.orm


import java.sql.{PreparedStatement, ResultSet}


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
   * DSL-like way to transform a column to foreign key association.
   */
  def references[P <: Record](referenceTable: Table[P]): ForeignKey[R, P] =
    table.foreignKey(referenceTable, this)

  /**
   * Extract a value from result set.
   */
  def read(rs: ResultSet, alias: String): Option[T]

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

  override def hashCode = this.table.hashCode * 31 + this.columnName.toLowerCase.hashCode
}

/**
 * String column (varchar-typed).
 */
class StringColumn[R <: Record](table: Table[R], name: String)
    extends Column[String, R](table, name, table.dialect.stringType) {
  def read(rs: ResultSet, alias: String) =
    Some(rs.getString(alias))
}

/**
 * Long column (bigint-typed).
 */
class LongColumn[R <: Record](table: Table[R], name: String)
    extends Column[Long, R](table, name, table.dialect.longType) {
  def read(rs: ResultSet, alias: String) =    // TODO check what happens if getLong returns null
    Some(rs.getLong(alias))
}


