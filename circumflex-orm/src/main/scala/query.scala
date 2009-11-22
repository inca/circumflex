package ru.circumflex.orm

import collection.mutable.ListBuffer
import java.sql.ResultSet

class Select extends Configurable with JDBCHelper {

  private var aliasCounter = 0;

  val projections = new ListBuffer[Projection[_]]
  val relations = new ListBuffer[RelationNode]

  private var _predicate: Predicate = EmptyPredicate
  private var _limit: Int = -1
  private var _offset: Int = 0

  def this(nodes: RelationNode *) = {
    this()
    relations ++= nodes.toList
    nodes.foreach(n => projections ++= n.projections)
  }

  /**
   * Wraps a table into RelationNode, assigning it a query-unique alias.
   */
  def makeNode(table: Table): TableNode = {
    val alias = "_" + table.relationName.take(3).mkString + aliasCounter
    aliasCounter += 1
    return new TableNode(table, alias)
  }

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
      (0 until _predicate.parameters.size).foreach(ix =>
          configuration.typeConverter.write(st, _predicate.parameters(ix), ix + 1))
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