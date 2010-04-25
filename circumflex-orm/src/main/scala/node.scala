package ru.circumflex.orm

import ORM._
import java.lang.String

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

  def AS(alias: String): this.type = as(alias)

  // ### Projections

  /**
   * Construct a new `RecordProjection` for this node.
   */
  def * = new RecordProjection[R](this)

  /**
   * Default projections for this node.
   */
  def projections: Seq[Projection[_]] = List(*)

  // ### Joins

  def findAssociation[F <: Record[F]](node: RelationNode[F]): Option[Association[R, F]] =
    relation.findAssociation(node.relation)

  /**
   * Explicit join.
   */
  def join[J <: Record[J]](node: RelationNode[J],
                           on: String,
                           joinType: JoinType): JoinNode[R, J] =
    new ExplicitJoin(this, node, on, joinType)

  /**
   * Auto-join (the `ON` subclause is evaluated by searching matching association).
   */
  def join[J <: Record[J]](node: RelationNode[J],
                           joinType: JoinType = LEFT_JOIN): JoinNode[R, J] =
    findAssociation(node) match {
      case Some(a: Association[R, J]) =>  // many-to-one join
        new ManyToOneJoin[R, J](this, node, a, joinType)
      case _ => node.findAssociation(this) match {
        case Some(a: Association[J, R]) =>  // one-to-many join
          new OneToManyJoin[R, J](this, node, a, joinType)
        case _ =>
          throw new ORMException("Failed to join " + this + " and " + node +
                  ": no associations found.")
      }
    }

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

  override def toString = toSql
}

// ## Proxy Node

/**
 * In order to organize joined nodes into tree we introduce this proxy
 * for `RelationNode`. It delegates all methods to underlying `node`.
 */
class ProxyNode[R <: Record[R]](protected[orm] var node: RelationNode[R])
        extends RelationNode[R](node.relation) {

  override def alias = node.alias
  override def as(alias: String): this.type = {
    node.as(alias)
    return this
  }

  override def projections = node.projections
  override def * = node.*

  override def equals(obj: Any) = node.equals(obj)
  override def hashCode = node.hashCode

  override def toSql = node.toSql

  /**
   * Unlike `clone` in `RelationNode` this creates a deep copy
   * (clones underlying `node`, but `relation` remains unchanged).
   */
  override def clone(): this.type = {
    val newNode = super.clone().asInstanceOf[this.type]
    val n = node.clone().asInstanceOf[RelationNode[R]]
    newNode.node = n
    return newNode
  }

}

// ## Joins

/**
 * This node represents a relational join between two nodes (`left` and `right`).
 */
abstract class JoinNode[L <: Record[L], R <: Record[R]](
        protected var _left: RelationNode[L],
        protected var _right: RelationNode[R],
        protected var _joinType: JoinType) extends ProxyNode[L](_left) {

  def left = _left
  def right = _right
  def joinType = _joinType

  /**
   * Join condition expression (used in `ON` subclauses).
   */
  def on: String

  def sqlOn = dialect.on(this.on)

  /**
   * Join node returns projections of `left` node appended with projections
   * of `right` node.
   */
  override def projections = left.projections ++ right.projections

  // ### Replacement methods

  def replaceLeft(newLeft: RelationNode[L]): this.type = {
    this._left = newLeft
    return this
  }

  def replaceRight(newRight: RelationNode[R]): this.type = {
    this._right = newRight
    return this
  }

  // ### Others

  override def toSql = dialect.join(this)

  /**
   * Creates a deep copy of this node, cloning left and right nodes.
   * The underlying relations of nodes remain unchanged.
   */
  override def clone(): this.type = super.clone()
          .replaceLeft(this.left.clone)
          .replaceRight(this.right.clone)
}

/**
 * A join with explicit join condition.
 */
class ExplicitJoin[L <: Record[L], R <: Record[R]](
        left: RelationNode[L],
        right: RelationNode[R],
        val on: String,
        joinType: JoinType) extends JoinNode[L, R](left, right, joinType)

/**
 * A join in many-to-one direction.
 */
class ManyToOneJoin[L <: Record[L], R <: Record[R]](
        childNode: RelationNode[L],
        parentNode: RelationNode[R],
        val association: Association[L, R],
        joinType: JoinType) extends JoinNode[L, R](childNode, parentNode, joinType) {
  def on = childNode.alias + "." + association.name + " = " +
          parentNode.alias + "." + association.foreignRelation.primaryKey.name
}

/**
 * A join in one-to-many direction.
 */
class OneToManyJoin[L <: Record[L], R <: Record[R]](
        parentNode: RelationNode[L],
        childNode: RelationNode[R],
        val association: Association[R, L],
        joinType: JoinType) extends JoinNode[L, R](parentNode, childNode, joinType) {
  def on = childNode.alias + "." + association.name + " = " +
          parentNode.alias + "." + association.foreignRelation.primaryKey.name
}
