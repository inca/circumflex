package ru.circumflex.orm

/**
 * Wraps relational nodes (tables, views, virtual tables, subqueries and other stuff)
 * with an alias so that they may appear within SQL FROM clause.
 */
abstract class RelationNode[R <: Record](val relation: Relation[R])
    extends Relation[R] with Configurable {

  def recordClass = relation.recordClass

  /**
   * Delegates to relation's configuration.
   */
  override def configuration = relation.configuration

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
   * Creates a join with specified node.
   */
  def join(node: RelationNode[_ <: Record]): JoinNode[_, _] =
    new JoinNode(this, node)


  override def toString = toSql

}

class TableNode[R <: Record](val table: Table[R],
                             var alias: String)
    extends RelationNode[R](table) {

  /**
   * Dialect should return qualified name with alias (e.g. "myschema.mytable as myalias")
   */
  def toSql = configuration.dialect.tableAlias(table, alias)

  /**
   * Table nodes define a collection of their column's projections.
   */
  def projections = List(new RecordProjection(this))
}

/**
 * Represents a join node between parent and child relation.
 */
class JoinNode[L <: Record, R <: Record](val leftNode: RelationNode[L],
                                         val rightNode: RelationNode[R])
    extends RelationNode[L](leftNode) {

  private var inverse: Boolean = false;

  /**
   * Evaluates an association between parent and child; throws an exception if
   * failed.
   */
  val association: Association[_ <: Record, _ <: Record] =
  leftNode.getParentAssociation(rightNode) match {
    case Some(a) => {
      this.inverse = true
      a
    } case None => leftNode.getChildAssociation(rightNode) match {
      case Some(a) => {
        this.inverse = false
        a
      } case None => throw new ORMException("Failed to join " + leftNode +
          " with " + rightNode + ": no associations found.")
    }
  }

  /**
   * Determines whether this join is "inverse", that is the child is joined against parent.
   * If parent is joined against child then this should yield <code>false</code>.
   */
  def isInverse: Boolean = inverse

  /**
   * Returns an alias of parent relation for this join.
   */
  def alias = leftNode.alias

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
  def projections = leftNode.projections ++ rightNode.projections
}