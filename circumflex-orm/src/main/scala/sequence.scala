package ru.circumflex.orm

/**
 * Base functionality for SQL sequences.
 */
class Sequence[R <: Record](val table: Table[R],
                            val column: Column[Long, R])
    extends SchemaObject {
  /* DDL */
  def dialect = table.dialect
  def sqlCreate = dialect.createSequence(this)
  def sqlDrop = dialect.dropSequence(this)
}