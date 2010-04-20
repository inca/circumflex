package ru.circumflex.orm

import ORM._

abstract class Constraint(val relation: Relation[_],
                          val constraintName: String)
        extends SchemaObject with SQLable {

  def objectName = constraintName

  def sqlCreate = dialect.alterTableAddConstraint(this)
  def sqlDrop = dialect.alterTableDropConstraint(this)

  def toSql = dialect.constraintDefinition(this)

  def sqlDefinition: String
}