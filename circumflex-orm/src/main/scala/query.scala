package ru.circumflex.orm

import collection.mutable.ListBuffer
import java.sql.ResultSet

class Select extends Configurable with JDBCHelper {

  private var aliasCounter = 0;

  val projections = new ListBuffer[Projection[_]]
  val relations = new ListBuffer[RelationNode]
  var predicate: Predicate = EmptyPredicate

  def this(nodes: RelationNode *) = {
    this()
    relations ++= nodes.toList
    nodes.foreach(n => projections ++= n.projections)
  }

  /**
   * Wraps a table into RelationNode, assigning it a query-unique alias.
   */
  protected def makeNode(table: Table): TableNode = {
    val alias = "_" + table.relationName.take(3).mkString + aliasCounter
    aliasCounter += 1
    return new TableNode(table, alias)
  }

  /**
   * Adds a table to FROM clause, first assigning it an alias.
   * All projections of specified table are also added to query projections list.
   */
  def addFrom(table: Table): Select = {
    val node = makeNode(table)
    relations ++= List(node)
    projections ++= node.projections
    this
  }

  /**
   * Joins specified table to the last relation in FROM clause,
   * first assigning it an alias, or throws an <code>ORMException<code>
   * if join is impossible. If no relations were previously added,
   * the specified table is just added to FROM clause
   * (as if <code>addFrom</code> been called).
   * All projections of specified table are also added to query projections list.
   */
  def addJoin(table: Table): Select =
    if (relations.size == 0) return addFrom(table)
    else {
      val node = makeNode(table)
      val joinNode = relations.last.join(node)
      relations(relations.size - 1) = joinNode
      projections ++= node.projections
      return this
    }

  /**
   * Sets WHERE clause of this query.
   */
  def where(predicate: Predicate): Select = {
    this.predicate = predicate
    return this
  }

  /**
   * Executes a query and Opens a JDBC result set.
   */
  def resultSet[A](actions: ResultSet => A): A =
    auto(configuration.connectionProvider.getConnection.prepareStatement(toSql))(st => {
      sqlLog.debug(toSql)
      (0 until predicate.parameters.size).foreach(ix =>
          configuration.typeConverter.write(st, predicate.parameters(ix), ix + 1))
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
   * Executes a query and returns first result.
   * The behavior is as follows: <ul>
   * <li>return <code>None</code> if result set yields no rows;</li>
   * <li>return <code>Some(tuple)</code> if result set yields one or more row
   * (first row is returned).</li>
   * </ul>
   */
  def first(): Option[Array[Any]] = resultSet(rs => {
    if (!rs.next) return None
    else return Some(projections.map(_.read(rs).getOrElse(null)).toArray)
  })

  def toSql = configuration.dialect.select(this)

  override def toString = toSql
}