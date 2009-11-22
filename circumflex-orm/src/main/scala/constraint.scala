package ru.circumflex.orm

/**
 * Base superclass with functionality for generating SQL constraints.
 */
abstract class Constraint(val table: Table)
    extends SchemaObject {

  /**
   * Constraint name (should be dialect-specific).
   */
  def constraintName: String

  def sqlCreate: String = dialect.alterTableAddConstraint(this)
  def sqlDrop: String = dialect.alterTableDropConstraint(this)

  /**
   * A "short" constraint definition (constraint-specific part only,
   * e.g. "PRIMARY KEY (id)" or "UNIQUE (name)", dialect-specific).
   */
  def sqlDefinition: String

  /**
   * Full SQL definition statement (should be dialect-specific,
   * e.g. "CONSTRAINT mytable_pkey PRIMARY KEY (id)".
   */
  def sqlFullDefinition = dialect.constraintDefinition(this)

  override def toString = sqlFullDefinition
}

/**
 * Primary key constraint.
 */
class PrimaryKey(table: Table,
                 val column: Column[_])
    extends Constraint(table) {
  def constraintName = table.dialect.primaryKeyName(this)
  def sqlDefinition = table.dialect.primaryKeyDefinition(this)
}

/**
 * Unique constraint.
 */
class UniqueKey(table: Table,
                val columns: Seq[Column[_]])
    extends Constraint(table) {
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
class ForeignKey(table: Table,
                 val referenceTable: Table,
                 val localColumn: Column[_])
    extends Constraint(table) with Association {

  def parentRelation = referenceTable
  def childRelation = table

  var onDelete: ForeignKeyAction = NoAction
  var onUpdate: ForeignKeyAction = NoAction

  def constraintName = table.dialect.foreignKeyName(this)
  def sqlDefinition = table.dialect.foreignKeyDefinition(this)

  def referenceColumn = referenceTable.primaryKey.column

  def onDeleteNoAction: ForeignKey = {
    onDelete = NoAction
    return this
  }

  def onDeleteCascade: ForeignKey = {
    onDelete = CascadeAction
    return this
  }

  def onDeleteRestrict: ForeignKey = {
    onDelete = RestrictAction
    return this
  }

  def onDeleteSetNull: ForeignKey = {
    onDelete = SetNullAction
    return this
  }

  def onDeleteSetDefault: ForeignKey = {
    onDelete = SetDefaultAction
    return this
  }

  def onUpdateNoAction: ForeignKey = {
    onUpdate = NoAction
    return this
  }

  def onUpdateCascade: ForeignKey = {
    onUpdate = CascadeAction
    return this
  }

  def onUpdateRestrict: ForeignKey = {
    onUpdate = RestrictAction
    return this
  }

  def onUpdateSetNull: ForeignKey = {
    onUpdate = SetNullAction
    return this
  }

  def onUpdateSetDefault: ForeignKey = {
    onUpdate = SetDefaultAction
    return this
  }

}
