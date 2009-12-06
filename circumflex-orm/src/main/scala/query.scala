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

import collection.mutable.ListBuffer
import java.sql.{PreparedStatement, ResultSet}

class Select extends Configurable with JDBCHelper with SQLable {
  private var aliasCounter = 0;

  private var _projections: Seq[Projection[_]] = Nil
  private var _relations: Seq[RelationNode[_]] = Nil
  private var _orders: Seq[Order] = Nil
  private var _predicate: Predicate = EmptyPredicate
  private var _limit: Int = -1
  private var _offset: Int = 0

  def this(nodes: RelationNode[_]*) = {
    this()
    nodes.toList.foreach(addFrom(_))
  }

  /**
   * Returns the WHERE clause of this query.
   */
  def where: Predicate = this._predicate

  /**
   * Returns the ORDER BY clause of this query.
   */
  def orders = _orders

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
    val n = if (node.alias == "this") {
      aliasCounter += 1
      node.as("this_" + aliasCounter)
    } else node
    this._relations ++= List(n)
    addProjection(node.projections: _*)
    return this
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
      case p: AliasedProjection[_] if (p.alias == "this") =>
        aliasCounter += 1
        p.as("this_" + aliasCounter)
      case p => p
    }
    return this
  }

  /**
   * Adds an order to ORDER BY clause.
   */
  def addOrder(order: Order*): this.type = {
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
   * Sets prepared statement params of this query starting from specified index.
   * Returns the new starting index of prepared statement.
   */
  def setParams(st: PreparedStatement, startIndex: Int): Int = {
    var paramsCounter = startIndex;
    _predicate.parameters.foreach(p => {
      typeConverter.write(st, p, paramsCounter)
      paramsCounter += 1
    })
    _orders.flatMap(_.parameters).foreach(p => {
      typeConverter.write(st, p, paramsCounter)
      paramsCounter += 1
    })
    return paramsCounter
  }


  /**
   * Executes a query and Opens a JDBC result set.
   */
  def resultSet[A](actions: ResultSet => A): A =
    auto(connectionProvider.getConnection.prepareStatement(toSql))(st => {
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
      val tuple = _projections.map(_.read(rs).getOrElse(null))
      result += tuple.toArray
    }
    return result
  })

  /**
   * Executes a query and returns a unique result.
   * An exception is thrown if result set yields more than one row.
   * </ul>
   */
  def unique(): Option[Array[Any]] = resultSet(rs => {
    if (!rs.next) return None
    else if (rs.isLast) return Some(_projections.map(_.read(rs).getOrElse(null)).toArray)
    else throw new ORMException("Unique result expected, but multiple rows found.")
  })

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

  def toSql = dialect.select(this)

}

/**
 * Represents an order for queries.
 */
class Order(val expression: String,
            val parameters: Seq[Any])

/**
 * Some common helpers for making up query-related .
 */
object Query extends Configurable {
  def conn = connectionProvider.getConnection

  /* NODE HELPERS */

  implicit def relationToNode[R](rel: Relation[R]): RelationNode[R] =
    rel.as("this")

  /* ORDER HELPERS */

  def asc(expr: String): Order = new Order(dialect.orderAsc(expr), Nil)

  def asc(proj: FieldProjection[_, _]): Order = asc(proj.expr)

  def desc(expr: String): Order = new Order(dialect.orderDesc(expr), Nil)

  def desc(proj: FieldProjection[_, _]): Order = desc(proj.expr)

  implicit def stringToOrder(expr: String): Order =
    new Order(expr, Nil)

  implicit def projectionToOrder(proj: FieldProjection[_, _]): Order =
    new Order(proj.expr, Nil)

  implicit def predicateToOrder(predicate: Predicate): Order =
    new Order(predicate.toSql, predicate.parameters)

  /* PREDICATE HELPERS */

  implicit def stringToHelper(str: String): SimpleExpressionHelper =
    new SimpleExpressionHelper(str)

  implicit def fieldProjectionToHelper(f: FieldProjection[_, _]): SimpleExpressionHelper =
    new SimpleExpressionHelper(f.expr)

  def and(predicates: Predicate*) =
    new AggregatePredicate(dialect.and, predicates.toList)

  def or(predicates: Predicate*) =
    new AggregatePredicate(dialect.or, predicates.toList)

  /* PROJECTION HELPERS */

  def scalar(expr: String) = new ScalarProjection[Any](expr, "this", false)

  implicit def stringToScalar(expr: String): ScalarProjection[Any] = scalar(expr)

  def count(expr: String) =
    new ScalarProjection[Int]("count(" + expr + ")", "this", true)
  def countDistinct(expr: String) =
    new ScalarProjection[Int]("count( distinct " + expr + ")", "this", true)
  def max(expr: String) =
    new ScalarProjection[Any]("max(" + expr + ")", "this", true)
  def min(expr: String) =
    new ScalarProjection[Any]("min(" + expr + ")", "this", true)
  def sum(expr: String) =
    new ScalarProjection[Any]("sum(" + expr + ")", "this", true)
  def avg(expr: String) =
    new ScalarProjection[Any]("avg(" + expr + ")", "this", true)

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