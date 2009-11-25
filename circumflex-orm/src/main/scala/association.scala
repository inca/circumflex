package ru.circumflex.orm

/**
 * Defines a contract for parent-child (one-to-many) associations.
 * Usually they are modelled as tables with foreign keys.
 */
trait Association[C, P] {

  import Query._

  /**
   * Returns child relation.
   */
  def childRelation: Relation[C]

  /**
   * Returns parent relation.
   */
  def parentRelation: Relation[P]

  /**
   * Returns a list of local columns.
   */
  def localColumn: Column[_, C]

  /**
   * Returns a column of the referenced table (parent) that matches local column.
   */
  def referenceColumn: Column[_, P] = parentRelation.primaryKey.column

  def fetchManyToOne(localValue: Any): Option[P] =
    parentRelation.createCriteria.add(_.field(referenceColumn).eq(localValue)).unique

  def fetchOneToMany(referenceValue: Any): Seq[C] =
    childRelation.createCriteria.add(_.field(localColumn).eq(referenceValue)).list

}
