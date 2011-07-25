package ru.circumflex
package orm

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
    this
  }

  /*! Relation nodes allow Scala-like syntactic sugar by using the `map` method.
  Following example gives table `City` an alias `ci` and uses it to construct a
  `Criteria` object.

      (City AS "ci").map(ci => ci.criteria.add(ci.name LIKE "Lausanne")).list
   */
  def map[T](f: RelationNode[PK, R] => T): T = f(this)

  /*! The `*` method creates a `RecordProjection` from this node and is
  widely used in the `SELECT` clause of SQL queries.
   */
  def * = new RecordProjection[PK, R](this)

  /*! Each relation node define which projection it yields. When nodes are joined
  the resulting node contains projections from both nodes.
   */
  def projections: Seq[Projection[_]] = List(*)

  /*! Creates new `Criteria` instance with this node as its query plan root. */
  def criteria = new Criteria[PK, R](this)

  /*! Relation nodes can be joined to allow restrictions of associated relations. */
  def findAssociation[T, F <: Record[T, F]](node: RelationNode[T, F]): Option[Association[T, R, F]] =
    this.relation.findAssociation(node.relation)

  def JOIN[T, J <: Record[T, J]](node: RelationNode[T, J],
                                 on: Expression,
                                 joinType: JoinType): JoinNode[PK, R, T, J] =
    new JoinNode(this, node, joinType).ON(on)
  def JOIN[T, J <: Record[T, J]](node: RelationNode[T, J],
                                 joinType: JoinType = LEFT): JoinNode[PK, R, T, J] =
    findAssociation(node) match {
      case Some(a: Association[T, R, J]) =>  // many-to-one join
        new ManyToOneJoin[PK, R, T, J](this, node, a, joinType)
      case _ => node.findAssociation(this) match {
        case Some(a: Association[PK, J, R]) =>  // one-to-many join
          new OneToManyJoin[PK, R, T, J](this, node, a, joinType)
        case _ =>
          new JoinNode(this, node, joinType).ON(EmptyPredicate)
      }
    }
  def INNER_JOIN[T, J <: Record[T, J]](node: RelationNode[T, J]): JoinNode[PK, R, T, J] =
    JOIN(node, INNER)
  def LEFT_JOIN[T, J <: Record[T, J]](node: RelationNode[T, J]): JoinNode[PK, R, T, J] =
    JOIN(node, LEFT)
  def RIGHT_JOIN[T, J <: Record[T, J]](node: RelationNode[T, J]): JoinNode[PK, R, T, J] =
    JOIN(node, RIGHT)
  def FULL_JOIN[T, J <: Record[T, J]](node: RelationNode[T, J]): JoinNode[PK, R, T, J] =
    JOIN(node, FULL)

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

  def toSql: String = ormConf.dialect.alias(relation.qualifiedName, alias)

  override def clone(): this.type = super.clone.asInstanceOf[this.type]

  override def toString: String = toSql
}

/*!## Implicit Convertions

`RelationNode` is implicitly converted to the `Relation` if necessary exposing
all the methods of the underlying relation. The alias is saved in a special
thread local stack so that it could become possible to refer to the alias
carried by this node, because otherwise it would be lost after conversion.

To understand this, consider following code:

    class User extends Record[...] {
      val id = "id".BIGINT
    }
    object User extends User[...] with Table[...] {...}

    val u = User AS "u"   // RelationNode of User with alias "u"
    SELECT(u.id).FROM(u)  // The `id` member does not exist in RelationNode,
                          // but it exists on the User, so the conversion works.
                          // But this eliminates the alias which will be refered
                          // by the projection. So it is saved into a stack and
                          // is recovered later.
*/
object RelationNode {
  implicit def toRelation[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): R = {
    aliasStack.push(node.alias)
    node.relation.asInstanceOf[R]
  }
}

/*! The `ProxyNode` wraps a node and provides functionality to arrange
joined nodes into a query plan (tree-like structure) by allowing to replace
an underlying `node` with it's equivalent `JoinNode`. Most methods delegate
to underlying `node`.
*/
class ProxyNode[PK, R <: Record[PK, R]](var node: RelationNode[PK, R])
    extends RelationNode[PK, R](node.relation) {

  override def alias = node.alias
  override def AS(alias: String): this.type = {
    node.AS(alias)
    this
  }

  override def projections = node.projections
  override def * = node.*

  override def canEqual(that: Any): Boolean = node.canEqual(that)
  override def equals(obj: Any) = node.equals(obj)
  override def hashCode = node.hashCode

  override def toSql = node.toSql

  override def clone(): this.type = {
    val newNode = super.clone().asInstanceOf[this.type]
    val n = node.clone().asInstanceOf[RelationNode[PK, R]]
    newNode.node = n
    newNode
  }

}

/*!# Joins

Relations can be joined within one query to allow applying restrictions on
associated relations. The `JoinNode` class represents a join between two relations.
We stick to a general convention called _left associativity_: two joined nodes
with equal left nodes are considered equal:

    (ci JOIN co) == ci
    (ci JOIN co JOIN ca) == ((ci JOIN co) JOIN ca)

This way you can compose arbitrary complex query plans. The join condition
(the `ON` subclause) can be either inferred from associations or specified
manually using the `ON` method.
*/
class JoinNode[PKL, L <: Record[PKL, L], PKR, R <: Record[PKR, R]](
    protected var _left: RelationNode[PKL, L],
    protected var _right: RelationNode[PKR, R],
    protected var _joinType: JoinType) extends ProxyNode[PKL, L](_left) {

  def left = _left
  def right = _right
  def joinType = _joinType

  protected var _on: Expression = EmptyPredicate
  def onClause = _on
  def ON(expr: Expression): this.type = {
    _on = expr
    this
  }

  def sqlOn = ormConf.dialect.on(this.onClause)

  override def projections = left.projections ++ right.projections

  def replaceLeft(newLeft: RelationNode[PKL, L]): this.type = {
    this._left = newLeft
    this
  }

  def replaceRight(newRight: RelationNode[PKR, R]): this.type = {
    this._right = newRight
    this
  }

  override def toSql = ormConf.dialect.join(this)

  override def clone(): this.type = super.clone()
      .replaceLeft(this.left.clone())
      .replaceRight(this.right.clone())

  override def toString = "(" + left + " -> " + right + ")"

}

class ManyToOneJoin[PKL, L <: Record[PKL, L], PKR, R <: Record[PKR, R]](
    childNode: RelationNode[PKL, L],
    parentNode: RelationNode[PKR, R],
    val association: Association[PKR, L, R],
    joinType: JoinType) extends JoinNode[PKL, L, PKR, R](childNode, parentNode, joinType) {
  override def onClause =
    if (_on == EmptyPredicate)
      association.joinPredicate(childNode.alias, parentNode.alias)
    else _on
}

class OneToManyJoin[PKL, L <: Record[PKL, L], PKR, R <: Record[PKR, R]](
    parentNode: RelationNode[PKL, L],
    childNode: RelationNode[PKR, R],
    val association: Association[PKL, R, L],
    joinType: JoinType) extends JoinNode[PKL, L, PKR, R](parentNode, childNode, joinType) {
  override def onClause =
    if (_on == EmptyPredicate)
      association.joinPredicate(childNode.alias, parentNode.alias)
    else _on
}