package ru.circumflex.orm


import collection.mutable.ListBuffer

/**
 * Base superclass with functionality for SQL constraints.
 * Constraint object must hold a reference to containing table
 * and to the list of columns. It is responsible for DDL generation.
 */
abstract class Constraint[R <: Record](val table: Table[R],
                                       val columns: Seq[Column[_, R]])
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
  def sqlFullDefinition: String = dialect.constraintDefinition(this)

}

/**
 * Primary key constraint.
 */
class PrimaryKey[R <: Record](table: Table[R],
                              columns: Seq[Column[_, R]])
    extends Constraint[R](table, columns) {
  def constraintName = table.dialect.primaryKeyName(this)
  def sqlDefinition = table.dialect.primaryKeyDefinition(this)
}

/**
 * Unique constraint.
 */
class UniqueKey[R <: Record](table: Table[R],
                             columns: Seq[Column[_, R]])
    extends Constraint[R](table, columns) {
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
    extends Constraint[C](table, columns) with Association[C, P] {

  private var _columnPairs: Seq[Pair[Column[_, P], Column[_, C]]] = Nil

  {   // init
    val localColSize = columns.size
    val refColSize = referenceTable.primaryKey.columns.size
    if (localColSize == 0)
      throw new ORMInitializationError("Failed to initialize foreign key " + constraintName +
          ": no local columns specified.")
    if (localColSize != refColSize)
      throw new ORMInitializationError("Failed to initialize foreign key " + constraintName +
          ": local columns count does not match referenced columns count.")
    (0 until localColSize).foreach(i =>
        _columnPairs ++= List(referenceTable.primaryKey.columns(i) -> columns(i))
      )
  }

  def parentRelation = referenceTable
  def childRelation = table

  var onDelete: ForeignKeyAction = NoAction
  var onUpdate: ForeignKeyAction = NoAction

  def constraintName = table.dialect.foreignKeyName(this)
  def sqlDefinition = table.dialect.foreignKeyDefinition(this)

  def columnPairs = _columnPairs

  def onDeleteNoAction: ForeignKey[C, P] = {
    onDelete = NoAction
    return this
  }

  def onDeleteCascade: ForeignKey[C, P] = {
    onDelete = CascadeAction
    return this
  }

  def onDeleteRestrict: ForeignKey[C, P] = {
    onDelete = RestrictAction
    return this
  }

  def onDeleteSetNull: ForeignKey[C, P] = {
    onDelete = SetNullAction
    return this
  }

  def onDeleteSetDefault: ForeignKey[C, P] = {
    onDelete = SetDefaultAction
    return this
  }

  def onUpdateNoAction: ForeignKey[C, P] = {
    onUpdate = NoAction
    return this
  }

  def onUpdateCascade: ForeignKey[C, P] = {
    onUpdate = CascadeAction
    return this
  }

  def onUpdateRestrict: ForeignKey[C, P] = {
    onUpdate = RestrictAction
    return this
  }

  def onUpdateSetNull: ForeignKey[C, P] = {
    onUpdate = SetNullAction
    return this
  }

  def onUpdateSetDefault: ForeignKey[C, P] = {
    onUpdate = SetDefaultAction
    return this
  }

}
