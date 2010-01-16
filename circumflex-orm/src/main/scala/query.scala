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

import ORM._
import collection.mutable.ListBuffer
import java.sql.{PreparedStatement, ResultSet}

/**
 * Result set operation (UNION, EXCEPT, INTERSECT, etc.).
 */
abstract class SetOperation(val expression: String)

object Union extends SetOperation("union")
object UnionAll extends SetOperation("union all")
object Except extends SetOperation("except")
object ExceptAll extends SetOperation("except all")
object Intersect extends SetOperation("intersect")
object IntersectAll extends SetOperation("intersect all")

trait Query extends SQLable with JDBCHelper {

  /**
   * Query parameters.
   */
  def parameters: Seq[Any]

  /**
   * Inlines query parameters and returns resulting SQL.
   */
  def toInlineSql: String = parameters.foldLeft(toSql)((sql, p) =>
    sql.replaceFirst("\\?", typeConverter.toString(p)))

  /**
   * Sets prepared statement parameters of this query starting from specified index.
   * Returns the new starting index of prepared statement.
   */
  def setParams(st: PreparedStatement, startIndex: Int): Int = {
    var paramsCounter = startIndex;
    parameters.foreach(p => {
      typeConverter.write(st, p, paramsCounter)
      paramsCounter += 1
    })
    return paramsCounter
  }

}

trait SQLQuery extends Query {

  def projections: Seq[Projection[_]]

  /**
   * Executes a query, opens a JDBC result set and executes provided actions.
   */
  def resultSet[A](actions: ResultSet => A): A = transactionManager.sql(toSql)(st => {
    sqlLog.debug(toSql)
    setParams(st, 1)
    auto(st.executeQuery)(actions)
  })

  /**
   * Executes a query and returns a list of tuples, designated by query projections.
   */
  def list(): Seq[Array[Any]] = resultSet(rs => {
    val result = new ListBuffer[Array[Any]]()
    while (rs.next) {
      val tuple = projections.map(_.read(rs).getOrElse(null))
      result += tuple.toArray
    }
    return result
  })

  /**
   * Executes a query and returns a unique result.
   * An exception is thrown if result set yields more than one row.
   */
  def unique(): Option[Array[Any]] = resultSet(rs => {
    if (!rs.next) return None
    else if (rs.isLast) return Some(projections.map(_.read(rs).getOrElse(null)).toArray)
    else throw new ORMException("Unique result expected, but multiple rows found.")
  })

}

class Subselect extends SQLQuery {

  protected var aliasCounter = 0;

  protected var _projections: Seq[Projection[_]] = Nil
  protected var _relations: Seq[RelationNode[_]] = Nil
  protected var _predicate: Predicate = EmptyPredicate
  protected var _setOps: Seq[Pair[SetOperation, Subselect]] = Nil

  def this(nodes: RelationNode[_]*) = {
    this()
    nodes.toList.foreach(addFrom(_))
  }

  /**
   * Returns query parameters sequence.
   */
  def parameters: Seq[Any] = _predicate.parameters ++
          _setOps.flatMap(p => p._2.parameters)

  /**
   * Returns queries combined with this subselect using specific set operation
   * (in pair, <code>SetOperation -> Subselect</code>),
   */
  def setOps = _setOps

  /**
   * Returns the WHERE clause of this query.
   */
  def where: Predicate = this._predicate

  /**
   * Returns the FROM clause of this query.
   */
  def relations = _relations

  /**
   * Returns the SELECT clause of this query.
   */
  def projections = _projections

  /**
   * Adds specified node to FROM clause.
   * All nodes with "this" alias are assigned query-unique alias.
   * All projections are added too.
   */
  def addFrom[R](node: RelationNode[R]): this.type = {
    ensureNodeAlias(node)
    this._relations ++= List(node)
    addProjection(node.projections: _*)
    return this
  }

  def ensureNodeAlias[R](node: RelationNode[R]): RelationNode[R] = node match {
    case j: JoinNode[_, _] =>
      ensureNodeAlias(j.left)
      ensureNodeAlias(j.right)
      j
    case n: RelationNode[_] if (n.alias == "this") =>
      aliasCounter += 1
      node.as("this_" + aliasCounter)
    case n => n
  }

  /**
   * Adds specified relation to FROM clause (assigning it query-unique alias).
   * All projections are added too.
   */
  def addFrom[R](rel: Relation[R]): this.type =
    addFrom(rel.as("this"))

  /**
   * Sets WHERE clause of this query.
   */
  def where(predicate: Predicate): this.type = {
    this._predicate = predicate
    return this
  }

  def clearProjections: this.type = {
    this._projections = Nil
    return this
  }

  /**
   * Adds specified projections to SELECT clause.
   * All projections with "this" alias are assigned query-unique alias.
   */
  def addProjection(projections: Projection[_]*): this.type = {
    this._projections ++= projections.toList.map {
      case p: AtomicProjection[_] => ensureProjectionAlias(p)
      case p: CompositeProjection[_] => {
        p.atomicProjections.foreach(ensureProjectionAlias(_))
        p
      }
      case p => p
    }
    return this
  }

  def ensureProjectionAlias[T](projection: AtomicProjection[T]): AtomicProjection[T] =
    if (projection.alias == "this") {
      aliasCounter += 1
      projection.as("this_" + aliasCounter)
    } else projection


  /* SET OPERATIONS */

  def addSetOp(op: SetOperation, subselect: Subselect): this.type = {
    _setOps ++= List(op -> subselect)
    return this
  }

  def union(subselect: Subselect): this.type =
    addSetOp(Union, subselect)

  def unionAll(subselect: Subselect): this.type =
    addSetOp(UnionAll, subselect)

  def except(subselect: Subselect): this.type =
    addSetOp(Except, subselect)

  def exceptAll(subselect: Subselect): this.type =
    addSetOp(ExceptAll, subselect)

  def intersect(subselect: Subselect): this.type =
    addSetOp(Intersect, subselect)

  def intersectAll(subselect: Subselect): this.type =
    addSetOp(IntersectAll, subselect)

  /* SQL */

  def toSubselectSql = dialect.subselect(this)

  def toSql = toSubselectSql

}

class Select extends Subselect {

  protected var _orders: Seq[Order] = Nil
  protected var _limit: Int = -1
  protected var _offset: Int = 0

  def this(nodes: RelationNode[_]*) = {
    this()
    nodes.toList.foreach(addFrom(_))
  }

  override def parameters: Seq[Any] = _predicate.parameters ++
          _setOps.flatMap(p => p._2.parameters)
  _orders.flatMap(_.parameters)

  /**
   * Returns the ORDER BY clause of this query.
   */
  def orders = _orders

  /**
   * Adds an order to ORDER BY clause.
   */
  def orderBy(order: Order*): this.type = {
    this._orders ++= order.toList
    return this
  }

  /**
   * Sets maximum results for this query. Use -1 for infinite-sized queries.
   */
  def limit(value: Int): this.type = {
    _limit = value
    return this
  }

  /**
   * Sets the offset for this query.
   */
  def offset(value: Int): this.type = {
    _offset = value
    return this
  }

  /**
   * Returns this query result limit.
   */
  def limit = this._limit

  /**
   * Returns this query result offset.
   */
  def offset = this._offset

  /**
   * Executes a query and returns the first result.
   * WARNING! This call implicitly sets the query limit to 1. If you plan to reuse
   * the query object after <code>first</code> is called, set query limit manually
   * or it will always yield a single row.
   * </ul>
   */
  def first(): Option[Array[Any]] = {
    limit(1)
    resultSet(rs => {
      if (!rs.next) return None
      else return Some(_projections.map(_.read(rs).getOrElse(null)).toArray)
    })
  }

  override def toSql = dialect.select(this)

}

/**
 * Represents an order for queries.
 */
class Order(val expression: String,
            val parameters: Seq[Any])

/**
 * Some common helpers for making up query-related stuff.
 */
trait QueryHelper {

  /* NODE HELPERS */

  implicit def relationToNode[R](rel: Relation[R]): RelationNode[R] =
    rel.as("this")

  /* ORDER HELPERS */

  def asc(expr: String): Order = new Order(dialect.orderAsc(expr), Nil)

  def asc(proj: ColumnProjection[_, _]): Order = asc(proj.expr)

  def desc(expr: String): Order = new Order(dialect.orderDesc(expr), Nil)

  def desc(proj: ColumnProjection[_, _]): Order = desc(proj.expr)

  implicit def stringToOrder(expr: String): Order =
    new Order(expr, Nil)

  implicit def projectionToOrder(proj: ColumnProjection[_, _]): Order =
    new Order(proj.expr, Nil)

  implicit def predicateToOrder(predicate: Predicate): Order =
    new Order(predicate.toSql, predicate.parameters)

  /* PREDICATE HELPERS */

  implicit def stringToHelper(str: String): SimpleExpressionHelper =
    new SimpleExpressionHelper(str)

  implicit def fieldProjectionToHelper(f: ColumnProjection[_, _]): SimpleExpressionHelper =
    new SimpleExpressionHelper(f.expr)

  def and(predicates: Predicate*) =
    new AggregatePredicate(dialect.and, predicates.toList)

  def or(predicates: Predicate*) =
    new AggregatePredicate(dialect.or, predicates.toList)

  def not(predicate: Predicate) =
    new SimpleExpression(dialect.not + " (" + predicate.toSql + ")", predicate.parameters)

  def exists(subselect: Subselect) =
    new SubqueryExpression("exists ", subselect)

  def notExists(subselect: Subselect) =
    new SubqueryExpression("not exists ", subselect)

  /* PROJECTION HELPERS */

  def scalar(expr: String) = new ScalarProjection[Any](expr, false)

  implicit def stringToScalar(expr: String): ScalarProjection[Any] = scalar(expr)

  def count(expr: String) =
    new ScalarProjection[Int]("count(" + expr + ")",  true)
  def countDistinct(expr: String) =
    new ScalarProjection[Int]("count( distinct " + expr + ")", true)
  def max(expr: String) =
    new ScalarProjection[Any]("max(" + expr + ")", true)
  def min(expr: String) =
    new ScalarProjection[Any]("min(" + expr + ")", true)
  def sum(expr: String) =
    new ScalarProjection[Any]("sum(" + expr + ")", true)
  def avg(expr: String) =
    new ScalarProjection[Any]("avg(" + expr + ")", true)

  /* QUERY HELPERS */

  def select(projections: Projection[_]*) = new SelectHelper(projections.toList)

  def update[R](rel: Relation[R]): Update[R] = new Update(rel)

  def delete[R](rel: Relation[R]): Delete[R] = new Delete(rel);

}

class SelectHelper(val projections: Seq[Projection[_]]) {

  def from(nodes: RelationNode[_]*): Select = {
    val q = new Select(nodes: _*)
    if (projections.size > 0) {
      q.clearProjections
      q.addProjection(projections: _*)
    }
    return q
  }

}