package ru.circumflex.orm

import ORM._

/**
 * Base superclass with functionality for generating SQL constraints.
 */
abstract class Constraint[R](val relation: Relation[R],
                             val constraintName: String)
    extends SchemaObject {

  def sqlCreate: String = dialect.alterTableAddConstraint(this)

  def sqlDrop: String = dialect.alterTableDropConstraint(this)

  def objectName = constraintName

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
abstract class PrimaryKey[T, R](relation: Relation[R],
                       constraintName: String)
    extends Constraint[R](relation, constraintName) {
  def column: Column[T, R]
}

/**
 * Primary key constraint with physical column.
 */
class PhysicalPrimaryKey[T, R](relation: Relation[R],
                               constraintName: String,
                               val column: Column[T, R])
    extends PrimaryKey[T, R](relation, constraintName) {
  def sqlDefinition = dialect.primaryKeyDefinition(this)
}

/**
 * Primary key for virtual relations (e.g. views), used to identify
 * virtual records.
 */
class VirtualPrimaryKey[R](relation: Relation[R],
                           constraintName: String)
    extends PrimaryKey[Nothing, R](relation, constraintName) {
  def column = throw new IllegalAccessException("Virtual keys do not have physical columns.")
  def sqlDefinition = throw new IllegalAccessException("Virtual keys do not have SQL definition.")
}


/**
 * Unique constraint.
 */
class UniqueKey[R](relation: Relation[R],
                   constraintName: String,
                   val columns: Seq[Column[_, R]])
    extends Constraint[R](relation, constraintName) {

  def sqlDefinition = dialect.uniqueKeyDefinition(this)
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

abstract class ForeignKey[C, P](val childRelation: Relation[C],
                                val parentRelation: Relation[P],
                                constraintName: String)
    extends Constraint[C](childRelation, constraintName) {

  protected var _onDelete: ForeignKeyAction = NoAction
  protected var _onUpdate: ForeignKeyAction = NoAction

  def sqlDefinition = dialect.foreignKeyDefinition(this)

  def onDelete = _onDelete
  def onUpdate = _onUpdate

  def childColumns: Seq[Column[_, C]]
  def parentColumns: Seq[Column[_, P]]

  def onDeleteNoAction: this.type = {
    _onDelete = NoAction
    return this
  }

  def onDeleteCascade: this.type = {
    _onDelete = CascadeAction
    return this
  }

  def onDeleteRestrict: this.type = {
    _onDelete = RestrictAction
    return this
  }

  def onDeleteSetNull: this.type = {
    _onDelete = SetNullAction
    return this
  }

  def onDeleteSetDefault: this.type = {
    _onDelete = SetDefaultAction
    return this
  }

  def onUpdateNoAction: this.type = {
    _onUpdate = NoAction
    return this
  }

  def onUpdateCascade: this.type = {
    _onUpdate = CascadeAction
    return this
  }

  def onUpdateRestrict: this.type = {
    _onUpdate = RestrictAction
    return this
  }

  def onUpdateSetNull: this.type = {
    _onUpdate = SetNullAction
    return this
  }

  def onUpdateSetDefault: this.type = {
    _onUpdate = SetDefaultAction
    return this
  }

}

/**
 * Represents multi-columned foreign key constraint.
 */
class MultiForeignKey[C, P](childRelation: Relation[C],
                            parentRelation: Relation[P],
                            constraintName: String,
                            val childColumns: Seq[Column[_, C]],
                            val parentColumns: Seq[Column[_, P]])
    extends ForeignKey[C, P](childRelation, parentRelation, constraintName)

/**
 * Represents single-columned foreign key constraint, which also acts like association.
 */
class AssociativeForeignKey[T, C, P](childRelation: Relation[C],
                                     parentRelation: Relation[P],
                                     constraintName: String,
                                     val childColumn: Column[T, C])
    extends ForeignKey[C, P](childRelation, parentRelation, constraintName)
        with Association[C, P] {

  val parentColumns = List(parentColumn)
  val childColumns = List(childColumn)

}

class CheckConstraint[R](relation: Relation[R],
                         constraintName: String,
                         val expression: String)
    extends Constraint[R](relation, constraintName) {

  def sqlDefinition = dialect.checkConstraintDefinition(this)
}
