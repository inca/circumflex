package ru.circumflex.orm

/**
 * Defines a contract for parent-child (one-to-many) associations.
 * Usually they are modelled as tables with foreign keys.
 */
trait Association[C, P] {

  import ORM._

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
  def childColumn: Column[_, C]

  /**
   * Alias for childColumn
   */
  def localColumn = childColumn

  /**
   * Returns a referenced (a.k.a. parent) column that matches local column.
   * In normal circumstances this matches parent's primary key.
   */
  def parentColumn: Column[_, P] = parentRelation.primaryKey.column

  /**
   * Alias for parentColumn
   */
  def referenceColumn = parentColumn

  def fetchManyToOne(localValue: Any): Option[P] = parentRelation
          .criteria
          .add(_.projection(parentColumn).eq(localValue))
          .unique

  def fetchOneToMany(referenceValue: Any): Seq[C] = childRelation
          .criteria
          .add(_.projection(childColumn).eq(referenceValue))
          .list

}
