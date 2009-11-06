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

  /**
   * Just proxies relation's primary key.
   */
  def primaryKey = relation.primaryKey

  /**
   * One or more projections that correspond to this node.
   */
  def projections: Seq[Projection[_]]

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

  /**
   * Proxies relation's qualified name.
   */
  def qualifiedName = relation.qualifiedName

  /**
   * Creates a join node with this node as a parent and specified child node.
   */
  def join[C <: Record](child: RelationNode[C]) = new JoinNode(this, child)

  override def toString = toSql

}

class TableNode[R <: Record](val table: Table[R],
                             var alias: String)
                            (implicit recordType: Manifest[R])
    extends RelationNode[R](table) {

  /**
   * Dialect should return qualified name with alias (e.g. "myschema.mytable as myalias")
   */
  def toSql = configuration.dialect.tableAlias(table, alias)

  /**
   * Table nodes define a collection of their column's projections.
   */
  def projections = table.columns.map(col => col.as(alias + "_" + col.columnName))
}

/**
 * Represents a join node between parent and child relation.
 */
class JoinNode[C <: Record, P <: Record](val parent: RelationNode[P],
                                         val child: RelationNode[C])
                                        (implicit recordType: Manifest[P])
    extends RelationNode[P](parent) {

  /**
   * Evaluates an association between parent and child; throws an exception if
   * failed.
   */
  val association: Association[C, P] = child.getParentAssociation(parent) match {
    case Some(a) => a
    case _ => throw new ORMException("Failed to join " + parent +
        " with " + child + ": no associations found.")
  }

  /**
   * Returns an alias of parent relation for this join.
   */
  def alias = parent.alias

  /**
   * Override join type if necessary.
   */
  def sqlJoinType: String = configuration.dialect.leftJoin

  /**
   * Dialect should return properly joined parent and child nodes.
   */
  def toSql = configuration.dialect.join(this)

  /**
   * Join nodes return parent node's projections joined with child node's ones.
   */
  def projections = parent.projections ++ child.projections
}