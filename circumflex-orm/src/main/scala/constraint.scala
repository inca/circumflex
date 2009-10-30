package ru.circumflex.orm

/**
 * Base superclass with functionality for SQL constraints.
 * Constraint object must hold a reference to containing table
 * and to the list of columns. It is responsible for DDL generation.
 */
abstract class Constraint[R <: Record](val table: Table[R],
                                       val columns: Seq[Column[_, R]]) {

  /**
   * Constraint name (should be dialect-specific).
   */
  def constraintName: String

  /**
   * SQL ALTER TABLE statement to create a constraint (dialect-specific).
   */
  def sqlCreate: String = table.dialect.alterTableAddConstraint(this)

  /**
   * SQL ALTER TABLE statement to drop a constraint (dialect-specific).
   */
  def sqlDrop: String = table.dialect.alterTableDropConstraint(this)

  /**
   * A "short" constraint definition (constraint-specific part only,
   * e.g. "PRIMARY KEY (id)" or "UNIQUE (name)", dialect-specific).
   */
  def sqlDefinition: String

  /**
   * Full SQL definition statement (should be dialect-specific,
   * e.g. "CONSTRAINT mytable_pkey PRIMARY KEY (id)".
   */
  def sqlFullDefinition: String = table.dialect.constraintDefinition(this)

}

/**
 * Primary key constraint.
 */
class PrimaryKey[R <: Record](table: Table[R],
                              columns: Seq[Column[_, R]]) extends Constraint[R](table, columns) {
  def constraintName = table.dialect.primaryKeyName(this)
  def sqlDefinition = table.dialect.primaryKeyDefinition(this)
}

/**
 * Unique constraint.
 */
class UniqueKey[R <: Record](table: Table[R],
                             columns: Seq[Column[_, R]]) extends Constraint[R](table, columns) {
  def constraintName = table.dialect.uniqueKeyName(this)
  def sqlDefinition = table.dialect.uniqueKeyDefinition(this)
}

/**
 * Marker interface for foreign key's ON DELETE and ON UPDATE actions.
 */
trait ForeignKeyAction

object NoAction extends ForeignKeyAction
object CascadeAction extends ForeignKeyAction
object RestrictAction extends ForeignKeyAction
object SetNullAction extends ForeignKeyAction
object SetDefaultAction extends ForeignKeyAction

/**
 * Foreign key constraint.
 */
class ForeignKey[C <: Record, P <: Record](table: Table[C],
                                           val referenceTable: Table[P],
                                           columns: Seq[Column[_, C]])
    extends Constraint[C](table, columns) {

  var onDelete: ForeignKeyAction = NoAction
  var onUpdate: ForeignKeyAction = NoAction

  def constraintName = table.dialect.foreignKeyName(this)
  def sqlDefinition = table.dialect.foreignKeyDefinition(this)

}
