package ru.circumflex.orm

import ORM._

// ## Schema Objects for DDL

// ### Constraints

abstract class Constraint(val relation: Relation[_],
                          val constraintName: String)
        extends SchemaObject with SQLable {

  def objectName = constraintName

  def sqlCreate = dialect.alterTableAddConstraint(this)
  def sqlDrop = dialect.alterTableDropConstraint(this)

  def toSql = dialect.constraintDefinition(this)

  def sqlDefinition: String
}

// ### Columns

class Column(val relation: Relation[_],
             var columnName: String,
             val sqlType: String,
             val nullable: Boolean,
             var default: Option[String])
    extends SQLable {

  def toSql = dialect.columnDefinition(this)

}