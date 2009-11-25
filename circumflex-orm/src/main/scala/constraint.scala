package ru.circumflex.orm

/**
 * Base superclass with functionality for generating SQL constraints.
 */
abstract class Constraint[R](val table: Table[R])
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
class PrimaryKey[T, R](table: Table[R],
                       val column: Column[T, R])
    extends Constraint[R](table) {
  def constraintName = table.dialect.primaryKeyName(this)

  def sqlDefinition = table.dialect.primaryKeyDefinition(this)
}

/**
 * Unique constraint.
 */
class UniqueKey[R](table: Table[R],
                   val columns: Seq[Column[_, R]])
    extends Constraint[R](table) {
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
class ForeignKey[T, C, P](table: Table[C],
                          val referenceTable: Table[P],
                          val localColumn: Column[T, C])
    extends Constraint[C](table) with Association[C, P] {
  def parentRelation = referenceTable

  def childRelation = table

  var onDelete: ForeignKeyAction = NoAction
  var onUpdate: ForeignKeyAction = NoAction

  def constraintName = table.dialect.foreignKeyName(this)

  def sqlDefinition = table.dialect.foreignKeyDefinition(this)

  def referenceColumn = referenceTable.primaryKey.column

  def onDeleteNoAction: ForeignKey[T, C, P] = {
    onDelete = NoAction
    return this
  }

  def onDeleteCascade: ForeignKey[T, C, P] = {
    onDelete = CascadeAction
    return this
  }

  def onDeleteRestrict: ForeignKey[T, C, P] = {
    onDelete = RestrictAction
    return this
  }

  def onDeleteSetNull: ForeignKey[T, C, P] = {
    onDelete = SetNullAction
    return this
  }

  def onDeleteSetDefault: ForeignKey[T, C, P] = {
    onDelete = SetDefaultAction
    return this
  }

  def onUpdateNoAction: ForeignKey[T, C, P] = {
    onUpdate = NoAction
    return this
  }

  def onUpdateCascade: ForeignKey[T, C, P] = {
    onUpdate = CascadeAction
    return this
  }

  def onUpdateRestrict: ForeignKey[T, C, P] = {
    onUpdate = RestrictAction
    return this
  }

  def onUpdateSetNull: ForeignKey[T, C, P] = {
    onUpdate = SetNullAction
    return this
  }

  def onUpdateSetDefault: ForeignKey[T, C, P] = {
    onUpdate = SetDefaultAction
    return this
  }

}
