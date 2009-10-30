package ru.circumflex.orm



abstract class Constraint[R <: Record](val table: Table[R],
                                       val columns: Seq[Column[_, R]]) {

  def constraintName: String

  def dialect = table.configuration.dialect

  def sqlCreate: String = dialect.alterTableAddConstraint(table, constraintName, sqlDefinition)

  def sqlDrop: String = dialect.alterTableDropConstraint(table, constraintName)

  def sqlDefinition: String

}


class PrimaryKey[R <: Record](table: Table[R],
                              columns: Seq[Column[_, R]]) extends Constraint[R](table, columns) {

  def constraintName = dialect.primaryKeyName(this)

  def sqlDefinition = dialect.primaryKeyDefinition(this)

}
