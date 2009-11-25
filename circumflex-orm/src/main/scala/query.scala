package ru.circumflex.orm

import collection.mutable.ListBuffer
import java.sql.ResultSet

class Select extends Configurable with JDBCHelper {
  private var aliasCounter = 0;

  val projections = new ListBuffer[Projection[_]]
  val relations = new ListBuffer[RelationNode[_]]
  val orders = new ListBuffer[Order]
  private var _predicate: Predicate = EmptyPredicate
  private var _limit: Int = -1
  private var _offset: Int = 0

  def this(nodes: RelationNode[_]*) = {
    this ()
    relations ++= nodes.toList
    nodes.foreach(n => projections ++= n.projections)
  }

  /**
   * Wraps a table into RelationNode, assigning it a query-unique alias.
   */
  def makeNode[R](rel: Relation[R]): RelationNode[R] = {
    val alias = "_" + rel.relationName.take(3).mkString + aliasCounter
    aliasCounter += 1
    return rel.as(alias)
  }

  /**
   * Adds specified node to FROM clause.
   * All projections are added too.
   */
  def addFrom[R](node: RelationNode[R]): Select = {
    this.relations += node
    this.projections ++= node.projections
    return this
  }

  /**
   * Adds specified table to FROM clause (assigning it query-unique alias).
   * All projections are added too.
   */
  def addFrom[R](table: Table[R]): Select =
    addFrom(makeNode(table))

  /**
   * Sets WHERE clause of this query.
   */
  def where(predicate: Predicate): Select = {
    this._predicate = predicate
    return this
  }

  /**
   * Returns the WHERE clause of this query.
   */
  def where: Predicate = this._predicate

  /**
   * Adds an order object to ORDER BY clause.
   */
  def addOrder(order: Order): Select = {
    this.orders += order
    return this
  }

  /**
   * Sets maximum results for this query. Use -1 for infinite-sized queries.
   */
  def limit(value: Int): Select = {
    _limit = value
    return this
  }

  /**
   * Sets the offset for this query.
   */
  def offset(value: Int): Select = {
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
   * Executes a query and Opens a JDBC result set.
   */
  def resultSet[A](actions: ResultSet => A): A =
    auto(configuration.connectionProvider.getConnection.prepareStatement(toSql))(st => {
      sqlLog.debug(toSql)
      var paramsCounter = 1;
      _predicate.parameters.foreach(p => {
        configuration.typeConverter.write(st, p, paramsCounter)
        paramsCounter += 1
      })
      orders.flatMap(_.parameters).foreach(p => {
        configuration.typeConverter.write(st, p, paramsCounter)
        paramsCounter += 1
      })
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
   * The behavior is as follows: <ul>
   * <li>return <code>None</code> if result set yields no rows;</li>
   * <li>return <code>Some(tuple)</code> if result set yields one and only one row;</li>
   * <li>throw <code>ORMException</code> if result set yields multiple rows.</li>
   * </ul>
   */
  def unique(): Option[Array[Any]] = resultSet(rs => {
    if (!rs.next) return None
    else if (rs.isLast) return Some(projections.map(_.read(rs).getOrElse(null)).toArray)
    else throw new ORMException("Unique result expected, but multiple rows found.")
  })

  /**
   * Executes a query and returns the first result.
   * The behavior is as follows: <ul>
   * <li>return <code>None</code> if result set yields no rows;</li>
   * <li>return <code>Some(tuple)</code> if result set yields one or more row
   * (first row is returned).</li>
   * </ul>
   */
  def first(): Option[Array[Any]] = {
    limit(1)
    resultSet(rs => {
      if (!rs.next) return None
      else return Some(projections.map(_.read(rs).getOrElse(null)).toArray)
    })
  }

  def toSql = configuration.dialect.select(this)

  override def toString = toSql
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
  def asc(expr: String): Order = new Order(configuration.dialect.orderAsc(expr), Nil)

  def asc(proj: FieldProjection[_, _]): Order = asc(proj.expr)

  def desc(expr: String): Order = new Order(configuration.dialect.orderDesc(expr), Nil)

  def desc(proj: FieldProjection[_, _]): Order = desc(proj.expr)

  implicit def stringToOrder(expr: String): Order =
    new Order(expr, Nil)

  implicit def projectionToOrder(proj: FieldProjection[_, _]): Order =
    new Order(proj.expr, Nil)

  implicit def predicateToOrder(predicate: Predicate): Order =
    new Order(predicate.toSql, predicate.parameters)

  implicit def stringTonHelper(str: String): SimpleExpressionHelper =
    new SimpleExpressionHelper(str)

  implicit def fieldProjectionToHelper(f: FieldProjection[_, _]): SimpleExpressionHelper =
    new SimpleExpressionHelper(f.expr)

  def and(predicates: Predicate*) =
    new AggregatePredicate(configuration.dialect.and, predicates.toList)

  def or(predicates: Predicate*) =
    new AggregatePredicate(configuration.dialect.or, predicates.toList)

  def select(nodes: RelationNode[_]*): Select = new Select(nodes: _*)

}