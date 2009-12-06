/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

package ru.circumflex.orm

/**
 * Wraps relational nodes (tables, views, virtual tables, subqueries and other stuff)
 * with an alias so that they may appear within SQL FROM clause.
 */
abstract class RelationNode[R](val relation: Relation[R],
                               var alias: String)
    extends Relation[R] with Configurable with SQLable {

  def recordClass = relation.recordClass

  /**
   * Just proxies relation's primary key.
   */
  def primaryKey = relation.primaryKey

  /**
   * One or more projections that correspond to this node.
   */
  def projections: Seq[Projection[_]]

  /**
   * Returns columns of underlying relation.
   */
  def columns = relation.columns

  /**
   * Retrieves an association path by delegating calls to underlying relations.
   */
  def getParentAssociation[P](parent: Relation[P]): Option[Association[R, P]] =
    parent match {
      case parentNode: RelationNode[P] => getParentAssociation(parentNode.relation)
      case _ => relation match {
        case childNode: RelationNode[R] => childNode.relation.getParentAssociation(parent)
        case _ => relation.getParentAssociation(parent)
      }
    }

  /**
   * Proxies relation's name.
   */
  def relationName = relation.relationName

  /**
   * Proxies relation's qualified name.
   */
  def qualifiedName = relation.qualifiedName

  /**
   * Creates a join with specified node.
   */
  def join[J](node: RelationNode[J]): JoinNode[R, J] =
    new JoinNode(this, node)

  /**
   * Returns associations defined on underlying relation.
   */
  def associations = relation.associations

  /**
   * Returns constraints defined on underlying relation.
   */
  def constraints = relation.constraints

  /**
   * Reassigns an alias for this node.
   */
  def as(alias: String): this.type = {
    this.alias = alias
    return this
  }

  /**
   * Creates a field projection with specified alias.
   */
  def field[T](col: Column[T, R], alias: String): FieldProjection[T, R] =
    new FieldProjection(this, col, alias)

  /**
   * Creates a field projection with default alias.
   */
  def field[T](col: Column[T, R]): FieldProjection[T, R] =
    new FieldProjection(this, col)

}

class TableNode[R](val table: Table[R],
                   alias: String)
    extends RelationNode[R](table, alias) {

  def this(table: Table[R]) = this(table, "this")

  /**
   * Dialect should return qualified name with alias (e.g. "myschema.mytable as myalias")
   */
  def toSql = dialect.tableAlias(table, alias)

  /**
   * Creates a record projection.
   */
  def projections = List(record)

  /**
   * Creates a record projection.
   */
  def record = new RecordProjection[R](this)

}

/**
 * Represents a join node between parent and child relation.
 */
class JoinNode[L, R](val leftNode: RelationNode[L],
                     val rightNode: RelationNode[R])
    extends RelationNode[L](leftNode, leftNode.alias) {
  private var inverse: Boolean = false;

  /**
   * Evaluates an association between parent and child; throws an exception if
   * failed.
   */
  val association: Association[_, _] = leftNode.getParentAssociation(rightNode) match {
    case Some(a) => {
      this.inverse = true
      a
    }
    case None => leftNode.getChildAssociation(rightNode) match {
      case Some(a) => {
        this.inverse = false
        a
      }
      case None => throw new ORMException("Failed to join " + leftNode +
          " with " + rightNode + ": no associations found.")
    }
  }

  /**
   * Determines whether this join is "inverse", that is the child is joined against parent.
   * If parent is joined against child then this should yield <code>false</code>.
   */
  def isInverse: Boolean = inverse

  /**
   * Override join type if necessary.
   */
  def sqlJoinType: String = dialect.leftJoin

  /**
   * Dialect should return properly joined parent and child nodes.
   */
  def toSql = dialect.join(this)

  /**
   * Join nodes return parent node's projections joined with child node's ones.
   */
  def projections = leftNode.projections ++ rightNode.projections

}