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

  def toSql = prepareQuery.toSql

  def prepareQuery: SQLQuery[Array[Any]] =
    SELECT(new UntypedTupleProjection(_projections: _*))
        .FROM(rootNode)         // TODO replace with full-blown query plan
        .WHERE(EmptyPredicate)  // TODO replace with evaluated predicate
        .ORDER_BY(_orders: _*)

}