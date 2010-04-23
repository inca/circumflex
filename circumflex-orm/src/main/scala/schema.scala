package ru.circumflex.orm

import ORM._
import ru.circumflex.core.CircumflexUtil._
import java.lang.reflect.Method

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
class UniqueKey(r: Relation[_], n: String, val fields: Seq[Field[_, _]])
    extends Constraint(r, n) {
  def sqlDefinition = dialect.uniqueKeyDefinition(this)
}

// ### Foreign Keys

case class ForeignKeyAction(val toSql: String) extends SQLable
