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

  private var _counter = 0

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

  /**
   * Attempt to search the root tree of query plan for relations of specified `association`
   * and correspondingly update it if necessary.
   */
  protected def updateRootTree[N <: Record[N], P <: Record[P], C <: Record[C]](
      node: RelationNode[N],
      association: Association[C, P]): RelationNode[N] =
    node match {
      case j: JoinNode[C, P] => j.replaceLeft(updateRootTree(j.left, association))
          .replaceRight(updateRootTree(j.right, association))
      case j: JoinNode[P, C] => j.replaceLeft(updateRootTree(j.left, association))
          .replaceRight(updateRootTree(j.right, association))
      case node: RelationNode[C] if (node.relation == association.record.relation) =>
        // N == C
        val a = association.asInstanceOf[Association[N, P]]
        new ManyToOneJoin(node, preparePf(a.foreignRelation, a), a, LEFT_JOIN)
      case node: RelationNode[P] if (node.relation == association.foreignRelation) =>
        // N == P
        val a = association.asInstanceOf[Association[C, N]]
        new OneToManyJoin(node, preparePf(a.record.relation, a), a, LEFT_JOIN)
      case node => node
    }

  /**
   * Prepare specified `node` and `association` to participate in prefetching.
   */
  protected def preparePf[N <: Record[N]](relation: Relation[N],
                                          association: Association[_, _]): RelationNode[N] = {
    _counter += 1
    val node = relation.as("pf_" + _counter)
    _projections ++= List(node.*)
    _prefetchSeq ++= List[Association[_,_]](association)
    return node
  }

  /**
   * Perform a depth-search and add specified `node` to specified `tree` of joins.
   */
  protected def updateJoinTree[N <: Record[N]](node: RelationNode[N],
                                               tree: RelationNode[R]): RelationNode[R] =
    tree match {
      case j: JoinNode[R, R] => try {   // try the left side
        j.replaceLeft(updateJoinTree(node, j.left))
      } catch {
        case e: ORMException =>         // try the right side
          j.replaceRight(updateJoinTree(node, j.right))
      }
      case rel: RelationNode[R] => rel.join(node)
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

  /**
   * Add specified `orders` to order specificators list.
   */
  def addOrder(orders: Order*): this.type = {
    _orders ++= orders.toList
    return this
  }

  /**
   * Add specified `association` to prefetch list.
   */
  def prefetch[P <: Record[P], C <: Record[C]](association: Association[C, P]): this.type = {
    if (!_prefetchSeq.contains(association)) {
      // The depth-search is used to update query plan if possible.
      _rootTree = updateRootTree(_rootTree, association)
      // TODO also process prefetch list of both sides of association.
    }
    return this
  }

  /**
   * Add specified `node` to join tree so that you can build queries with transitive criteria.
   */
  def addJoin[N <: Record[N]](node: RelationNode[N]): this.type = {
    _joinTree = updateJoinTree(node, _joinTree)
    return this
  }

}