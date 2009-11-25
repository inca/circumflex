package ru.circumflex.orm

import collection.mutable.ListBuffer
import Query._

/**
 * Criteria is high-level abstraction of <code>Select</code>. It is used to operate
 * specifically with records and simplifies record querying.
 * Note that if you want to use projections, you should still use lower-level <code>Select</code>
 */
class Criteria[R](val relation: Relation[R]) {

  protected val query: Select = select()
  protected val rootNode: RelationNode[R] = query.makeNode(relation)
  protected val restrictions = new ListBuffer[Predicate]()
  


}