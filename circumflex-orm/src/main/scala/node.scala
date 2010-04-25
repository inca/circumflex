package ru.circumflex.orm

import ORM._

// ## Relation Node

/**
 * **Relation Node** is essentially a wrapper around `Relation` that
 * provides an `alias` so that it can be used in SQL queries.
 */
class RelationNode[R <: Record[R]](val relation: Relation[R])
        extends SQLable with Cloneable {

  // ### Alias

  protected[orm] var _alias = "this"

  /**
   * An alias of this node. `this` is expanded to query-unique alias.
   */
  def alias = _alias

  /**
   * Change the alias of this node.
   */
  def as(alias: String): this.type = {
    this._alias = alias
    return this
  }

  // ### Projections

  /**
   * Construct a new `RecordProjection` for this node.
   */
  def * = new RecordProjection[R](this)

  /**
   * Default projections for this node.
   */
  def projections: Seq[Projection[_]] = List(*)

  // ### Equality and others

  override def equals(obj: Any) = obj match {
    case r: RelationNode[_] =>
      r.relation == this.relation && r.alias == this.alias
    case _ => false
  }

  override def hashCode = this.relation.hashCode * 31 + this.alias.hashCode

  /**
   * Creates a shallow copy of this node.
   * The underlying relation remains unchanged.
   */
  override def clone(): this.type = super.clone.asInstanceOf[this.type]

  def toSql = dialect.alias(relation.qualifiedName, alias)
}