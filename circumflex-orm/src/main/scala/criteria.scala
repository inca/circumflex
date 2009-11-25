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

  /**
   * Sets the maximum results for this query.
   */
  def limit(value: Int): this.type = {
    query.limit(value)
    return this
  }

  /**
   * Sets the result offset for this query.
   */
  def offset(value: Int): this.type = {
    query.offset(value)
    return this
  }

  /**
   * Adds arbitrary predicate to this query's restrictions list.
   * Restrictions are assembled into conjunction prior to query execution.
   */
  def add(predicate: Predicate): this.type = {
    restrictions += predicate
    return this
  }

  /**
   * Adds a predicate related to criteria root node to this query's restrictions list.
   * Restrictions are assembled into conjunction prior to query execution.
   */
  def add(nodeToPredicate: RelationNode[R] => Predicate): this.type = {
    restrictions += nodeToPredicate(rootNode)
    return this
  }

  /**
   * Adds an order to ORDER BY clause.
   */
  def addOrder(order: Order): this.type = {
    query.addOrder(order)
    return this
  }

  protected def prepareQuery: Select =
    query.where(if (restrictions.size > 0) and(restrictions: _*) else EmptyPredicate)

  /**
   * Executes the query and retrieves records list.
   */
  def list: Seq[R] = prepareQuery.list.map(_.apply(0).asInstanceOf[R])

  /**
   * Executes the query and retrieves unqiue record.
   * An exception is thrown if result set yields more than one row.
   */
  def unique: Option[R] = prepareQuery.unique.map(_.apply(0).asInstanceOf[R])

  /**
   * Executes a query and retrieves the first record.
   * WARNING! This call implicitly sets the query limit to 1. If you plan to reuse
   * the criteria object after <code>first</code> is called, set query limit manually
   * or it will always yield a single row.
   */
  def first: Option[R] = prepareQuery.unique.map(_.apply(0).asInstanceOf[R])

}