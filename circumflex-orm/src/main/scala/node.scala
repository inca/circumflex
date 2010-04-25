package ru.circumflex.orm

import ORM._

// ## Relation Node

/**
 * **Relation Node** is essentially a wrapper around `Relation` that
 * provides an `alias` so that they can be used in SQL queries.
 */
abstract class RelationNode[R <: Record[R]](val relation: Relation[R])
        extends Relation[R] with SQLable with Cloneable {

  protected[orm] var _alias = "this"

  /**
   * An alias of this node. `this` is expanded to query-unique alias.
   */
  def alias = _alias

  /**
   * Construct a new `RecordProjection` for this node.
   */
  def * = new RecordProjection[R](this)

}