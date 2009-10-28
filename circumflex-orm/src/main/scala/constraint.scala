package ru.circumflex.orm



abstract class Constraint (val table: Table) {

  var columns: Seq[Column[_]] = Nil

  def constraintName: String

  def dialect = table.configuration.dialect

  def sqlCreate: String = dialect.alterTableAddConstraint(table, constraintName, sqlDefinition)

  def sqlDrop: String = dialect.alterTableDropConstraint(table, constraintName)

  def sqlDefinition: String

}


class PrimaryKey(table: Table) extends Constraint(table, name) {

  def constraintName = dialect.primaryKeyName(this)

  def sqlDefinition = dialect.primaryKeyDefinition(this)

}
