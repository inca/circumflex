package ru.circumflex.orm

/**
 * Base functionality for SQL sequences.
 */
class Sequence[R <: Record, T <: Number](val table: Table[R],
                                         val column: Column[T, R])
    extends SchemaObject {
  /* DDL */
  def dialect = table.dialect
  def sqlCreate = dialect.createSequence(this)
  def sqlDrop = dialect.dropSequence(this)
}