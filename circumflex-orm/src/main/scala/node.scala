package ru.circumflex.orm

import java.lang.String

/*!# Relation Nodes

The `RelationNode` is essentially a wrapper around relation which provides
an `alias` so that it can participate in complex SQL query plans.

Generally a relation node is created implicitly from `Relation`.
A special alias `this` is assigned to new relation nodes, it is transformed
inside queries into a query-unique alias so that no collisions occur.

You may assign an alias explicitly using the `AS` method.
*/
class RelationNode[PK, R <: Record[PK, R]](val relation: Relation[PK, R])
    extends SQLable with Cloneable with Equals {

  protected var _alias: String = "this"
  def alias = _alias
  def AS(alias: String): this.type = {
    this._alias = alias
    return this
  }

  /*! The `*` method creates a `RecordProjection` from this node and is
  widely used in the `SELECT` clause of SQL queries.
   */
  def * = new RecordProjection[PK, R](this)

  /*! Each relation node define which projection it yields. When nodes are joined
  the resulting node contains projections from both nodes.
   */
  def projections: Seq[Projection[_]] = List(*)

  // TODO add criteria and joins

  /*!## Equality & Others

  Two nodes are considered equal if they wrap the same relation and share
  same aliases.

  The `hashCode` method delegates to node's relation.

  The `canEqual` method indicates that two nodes wrap the same relation.

  The `clone` methods creates a shallow copy of this node (the underlying
  relation remains unchanged).

  Finally, both `toSql` and `toString` return dialect specific SQL expression
  which appends an alias to relation's qualified name.
  */

  override def equals(that: Any): Boolean = that match {
    case that: RelationNode[_, _] =>
      this.canEqual(that) && this.alias == that.alias
    case _ => false
  }

  override def hashCode: Int = relation.hashCode

  def canEqual(that: Any): Boolean = that match {
    case that: RelationNode[_, _] =>
      this.relation == that.relation
  }

  def toSql: String = dialect.alias(relation.qualifiedName, alias)

  override def clone(): this.type = super.clone.asInstanceOf[this.type]

  override def toString: String = toSql
}

/*! The `ProxyNode` wraps a node and provides functionality to arrange
joined nodes into a query plan (tree-like structure). Most methods delegate
to underlying `node`.
*/
class ProxyNode[PK, R <: Record[PK, R]](protected[orm] var node: RelationNode[PK, R])
    extends RelationNode[PK, R](node.relation) {

  override def alias = node.alias
  override def AS(alias: String): this.type = {
    node.AS(alias)
    return this
  }

  override def projections = node.projections
  override def * = node.*

  override def canEqual(that: Any): Boolean = node.canEqual(that)
  override def equals(obj: Any) = node.equals(obj)
  override def hashCode = node.hashCode

  override def toSql = node.toSql

  /**
   * Unlike `clone` in `RelationNode` this creates a deep copy (clones underlying
   * `node`, but `relation` remains unchanged).
   */
  override def clone(): this.type = {
    val newNode = super.clone().asInstanceOf[this.type]
    val n = node.clone().asInstanceOf[RelationNode[PK, R]]
    newNode.node = n
    return newNode
  }

}