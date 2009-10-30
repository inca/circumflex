package ru.circumflex.orm



abstract class Constraint[R <: Record](val table: Table[R],
                                       val columns: Seq[Column[_, R]]) {

  def constraintName: String

  def sqlCreate: String = table.dialect.alterTableAddConstraint(this)

  def sqlDrop: String = table.dialect.alterTableDropConstraint(this)

  def sqlDefinition: String

  def sqlFullDefinition: String = table.dialect.constraintDefinition(this)

}


class PrimaryKey[R <: Record](table: Table[R],
                              columns: Seq[Column[_, R]]) extends Constraint[R](table, columns) {

  def constraintName = table.dialect.primaryKeyName(this)

  def sqlDefinition = table.dialect.primaryKeyDefinition(this)

}

class UniqueKey[R <: Record](table: Table[R],
                             columns: Seq[Column[_, R]]) extends Constraint[R](table, columns) {

  def constraintName = table.dialect.uniqueKeyName(this)

  def sqlDefinition = table.dialect.uniqueKeyDefinition(this)

}
