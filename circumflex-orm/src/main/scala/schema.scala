package ru.circumflex.orm

import ORM._

// ## Schema Objects for DDL

// ### Constraints

/**
 * Common stuff for all constraints.
 */
abstract class Constraint(val relation: Relation[_],
                          val constraintName: String)
        extends SchemaObject with SQLable {

  def objectName = constraintName

  def sqlCreate = dialect.alterTableAddConstraint(this)
  def sqlDrop = dialect.alterTableDropConstraint(this)

  def toSql = dialect.constraintDefinition(this)

  def sqlDefinition: String
}

/**
 * An SQL `UNIQUE` constraint.
 */
class UniqueKey(r: Relation[_], n: String, val fields: Seq[Field[_]])
    extends Constraint(r, n) {
  def sqlDefinition = dialect.uniqueKeyDefinition(this)
}

class ForeignKey(r: Relation[_],
                 n: String,
                 val foreignRelation: Relation[_],
                 val localFields: Seq[Field[_]],
                 val foreignFields: Seq[Field[_]],
                 val onDelete: ForeignKeyAction,
                 val onUpdate: ForeignKeyAction) extends Constraint(r, n) {
  def sqlDefinition = dialect.foreignKeyDefinition(this)
}
