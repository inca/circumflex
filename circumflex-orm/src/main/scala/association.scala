package ru.circumflex.orm

/**
 * Defines a contract for parent-child (one-to-many) associations.
 * Usually they are modelled as tables with foreign keys.
 */
trait Association[C <: Record, P <: Record] {
  /**
   * Returns parent relation.
   */
  def parentRelation: Relation[P]

  /**
   * Returns child relation.
   */
  def childRelation: Relation[C]

  /**
   * Returns columns pairs to use in joining operations.
   * The first column in pair is from parent table, the second a matching one from child table.
   */
  def columnPairs: Seq[Pair[Column[_, P], Column[_, C]]]

  /**
   * Instantiates a field, proxied by this association.
   */
  def apply(): AssociationParentField[P]
}
