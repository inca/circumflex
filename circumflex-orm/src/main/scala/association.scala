package ru.circumflex.orm

/**
 * Defines a contract for parent-child (one-to-many) associations.
 * Usually they are modelled as tables with foreign keys.
 */
trait Association {
  /**
   * Returns parent relation.
   */
  def parentRelation: Relation

  /**
   * Returns child relation.
   */
  def childRelation: Relation

  /**
   * Returns a list of local columns.
   */
  def localColumn: Column[_]

  /**
   * Returns a column of the referenced table (parent) that matches local column.
   */
  def referenceColumn: Column[_]

}
