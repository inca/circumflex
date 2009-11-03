package ru.circumflex.orm

import reflect.Manifest

/**
 * Adds an alias to various stuff.
 */
trait Aliased {
  def alias: String
}

/**
 * Designates a relation that can be used to recover certain type of records.
 * It can be considered a table, a virtual table, a view, a subquery or everything
 * that may participate in FROM clause.
 */
abstract class Relation[R <: Record](implicit recordType: Manifest[R]) {
  /**
   * A record class recovered from type parameter.
   */
  def recordClass: Class[R] = Class.forName(recordType.toString).asInstanceOf[Class[R]]
}

/**
 * Represents a join relation. Join relation holds is a collection of associations between
 * a single parent relation and one or more children. It's type parameter refers to parent
 * relation record.
 */
class JoinRelation[P <: Record](implicit recordType: Manifest[P],
                                val parentRelation: Relation[P] with Aliased,
                                childrenAssociations: Association[_, P] with Aliased *)
    extends Relation[P] with Aliased {

  /**
   * Returns an alias of parent relation for this join.
   */
  def alias = parentRelation.alias

}