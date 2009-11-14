package ru.circumflex.orm

/**
 * Base functionality for SQL sequences.
 */
class Sequence(val table: Table,
               val column: Column[Long])
    extends SchemaObject {
  /* DDL */
  def sqlCreate = dialect.createSequence(this)
  def sqlDrop = dialect.dropSequence(this)
}