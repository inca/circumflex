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
   * Returns a local (a.k.a. child) column.
   */
  def localColumn: Column[_, C]

  /**
   * Returns a referenced (a.k.a. parent) column that matches local column.
   * In normal circumstances this matches parent's primary key.
   */
  def referenceColumn: Column[_, P] = parentRelation.primaryKey.column

  def fetchManyToOne(localValue: Any): Option[P] =
    parentRelation.createCriteria.add(_.field(referenceColumn).eq(localValue)).unique

  def fetchOneToMany(referenceValue: Any): Seq[C] =
    childRelation.createCriteria.add(_.field(localColumn).eq(referenceValue)).list

}
