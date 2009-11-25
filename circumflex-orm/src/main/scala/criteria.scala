package ru.circumflex.orm

import collection.mutable.ListBuffer
import Query._

/**
 * Criteria is high-level abstraction of <code>Select</code>. It is used to operate
 * specifically with records and simplifies record querying.
 * Note that if you want to use projections, you should still use lower-level <code>Select</code>
 */
class Criteria[R](val relation: Relation[R]) {

  protected val query: Select = select().addFrom(relation)
  protected val restrictions = new ListBuffer[Predicate]()

  def rootNode: RelationNode[R] = query.relations(0).asInstanceOf[RelationNode[R]]

  def limit(value: Int): this.type = {
    query.limit(value)
    return this
  }

  def offset(value: Int): this.type = {
    query.offset(value)
    return this
  }

  def add(predicate: Predicate): this.type = {
    restrictions += predicate
    return this
  }

  def add(nodeToPredicate: RelationNode[R] => Predicate): this.type = {
    restrictions += nodeToPredicate(rootNode)
    return this
  }

  def addOrder(order: Order): this.type = {
    query.addOrder(order)
    return this
  }

  protected def prepareQuery: Select =
    query.where(if (restrictions.size > 0) and(restrictions: _*) else EmptyPredicate)

  def list: Seq[R] = prepareQuery.list.map(_.apply(0).asInstanceOf[R])
  
  def unique: Option[R] = prepareQuery.unique.map(_.apply(0).asInstanceOf[R])

  def first: Option[R] = prepareQuery.unique.map(_.apply(0).asInstanceOf[R])

}