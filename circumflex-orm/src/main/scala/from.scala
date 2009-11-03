package ru.circumflex.orm

/**
 * Defines a contract for tables, views, virtual tables, subqueries and other relations
 * (called nodes) that may appear within SQL FROM clause.
 */
trait FromNode extends Configurable {
  /**
   * An alias to refer the node from projections and criteria.
   */
  def alias: String
  /**
   * SQL representation of this node for FROM clause.
   */
  def sqlFrom: String
}

class TableNode[R <: Record](val table: Table[R],
                             var alias: String) extends FromNode {
  def sqlFrom = configuration.dialect.tableAlias(table, alias)
}

/**
 * Represents a join node. Join holds associations from a single parent relation
 * to one or more children relations. It's type parameter refers to parent relation record.
 */
class JoinNode[P <: Record](val parentRelation: Relation[P] with FromNode,
                            val childrenAssociations: Seq[Association[_, P] with FromNode])
    extends FromNode {

  /**
   * Returns an alias of parent relation for this join.
   */
  def alias = parentRelation.alias

  /**
   * Override join type if necessary.
   */
  def sqlJoinType: String = configuration.dialect.leftJoin

  def sqlFrom = configuration.dialect.join(this)
}