package ru.circumflex.orm


import reflect.Manifest

/**
 * Wraps relational nodes (tables, views, virtual tables, subqueries and other stuff)
 * with an alias so that they may appear within SQL FROM clause.
 */
abstract class RelationNode[R <: Record](val relation: Relation[R])
                                    (implicit recordType: Manifest[R])

    extends Relation[R] with Configurable {

  /**
   * An alias to refer the node from projections and criteria.
   */
  def alias: String

  /**
   * SQL representation of this node for use in FROM clause.
   */
  def toSql: String

  def primaryKey = relation.primaryKey

  /**
   * Retrieves an association path by delegating calls to underlying relations.
   */
  def getParentAssociation[P <: Record](parent: Relation[P]): Option[Association[R, P]] =
    parent match {
      case parentNode: RelationNode[P] => getParentAssociation(parentNode.relation)
      case _ => relation match {
        case childNode: RelationNode[R] => childNode.relation.getParentAssociation(parent)
        case _ => relation.getParentAssociation(parent)
      }
    }

  def qualifiedName = relation.qualifiedName

  override def toString = toSql

  def join[C <: Record](child: RelationNode[C]) = new JoinNode(this, child)
  
}

class TableNode[R <: Record](val table: Table[R],
                             var alias: String)
                            (implicit recordType: Manifest[R])
    extends RelationNode[R](table) {

  def toSql = configuration.dialect.tableAlias(table, alias)

}

/**
 * Represents a join node between parent and child relation.
 */
class JoinNode[C <: Record, P <: Record](val parent: RelationNode[P],
                                         val child: RelationNode[C])
                                        (implicit recordType: Manifest[P])
    extends RelationNode[P](parent) {

  /**
   * Returns an alias of parent relation for this join.
   */
  def alias = parent.alias

  /**
   * Override join type if necessary.
   */
  def sqlJoinType: String = configuration.dialect.leftJoin

  def toSql = configuration.dialect.join(this)

}