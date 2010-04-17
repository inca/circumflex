package ru.circumflex.orm

/**
 * **Associations** are used to link relational model classes into
 * the graphs of dependent objects. The associations are typically
 * expressed with `ForeignKey` constraints, although, some association
 * types require additional `UniqueKey` constraints or even intermediate
 * tables.
 */
trait Association[C <: Relation[C], P <: Relation[P]] {
  def childRelation: C
  def parentRelation: P
}

