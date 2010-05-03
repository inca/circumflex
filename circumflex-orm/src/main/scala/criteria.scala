package ru.circumflex.orm

import ORM._

// ## Criteria API

/**
 * **Criteria API** is a simplified queriyng interface which allows
 * to fetch records in neat object-oriented notation with the ability to
 * fetch the whole hierarchy of records in one query via *prefetching*.
 *
 * Criteria API is designed to operate on `Record`s specifically. If
 * you want to use different projections, use `Select` instead.
 */
class Criteria[R <: Record[R]](val rootNode: RelationNode[R])
    extends SQLable with Cloneable {

  private var _country = 0

  protected var _rootTree: RelationNode[R] = rootNode
  protected var _joinTree: RelationNode[R] = rootNode
  protected var _prefetchSeq: Seq[Association[_, _]] = Nil

  protected var _projections: Seq[RecordProjection[_]] = List(rootNode.*)
  protected var _restrictions: Seq[Predicate] = Nil
  protected var _orders: Seq[Order] = Nil
  protected var _limit = -1
  protected var _offset = 0

  // ### Internal stuff

  def toSql = prepareQuery.toSql

  /**
   * Prepare an actual query.
   */
  protected def prepareQuery: SQLQuery[Array[Any]] =
    SELECT(new UntypedTupleProjection(_projections: _*))
        .FROM(prepareQueryPlan)
        .WHERE(preparePredicate)
        .ORDER_BY(_orders: _*)

  /**
   * Compose an actual predicate from restrictions.
   */
  protected def preparePredicate: Predicate =
    if (_restrictions.size > 0)
      AND(_restrictions: _*)
    else EmptyPredicate

  /**
   * Merge the *join tree* with *prefetch tree* to form an actual `FROM` clause.
   */
  protected def prepareQueryPlan: RelationNode[R] = _joinTree match {
    case j: JoinNode[R, _] => replaceLeft(j.clone, _rootTree)
    case r: RelationNode[R] => _rootTree
  }

  /**
   * Replace left-most node of specified `join` with specified `node`.
   */
  protected def replaceLeft(join: JoinNode[R, _],
                            node: RelationNode[R]): RelationNode[R] =
    join.left match {
      case j: JoinNode[R, _] => replaceLeft(j, node)
      case r: RelationNode[R] => join.replaceLeft(node)
    }

  // ## Public Stuff

  /**
   * Add specified `predicates` to restrictions list.
   */
  def add(predicates: Predicate*): this.type = {
    _restrictions ++= predicates.toList
    return this
  }
  def add(expression: String, params: Pair[String, Any]*): this.type =
    add(prepareExpr(expression, params: _*))

  def addOrder(orders: Order*): this.type = {
    _orders ++= orders.toList
    return this
  }

}