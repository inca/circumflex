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

  /*!## Equality & Others

  Two nodes are considered equal if they wrap the same relation and share
  same aliases.

  The `hashCode` method delegates to node's relation.

  The `canEqual` method indicates that two nodes wrap the same relation.

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

  override def toString: String = toSql
}
