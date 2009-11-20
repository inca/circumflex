package ru.circumflex.orm

import collection.mutable.ListBuffer
import java.sql.ResultSet

class Select extends Configurable with JDBCHelper {

  var projections: Seq[Projection[_]] = Nil
  var relations: Seq[RelationNode] = Nil
  var predicate: Predicate = EmptyPredicate

  def this(nodes: RelationNode *) = {
    this()
    relations = nodes.toList
    nodes.foreach(n => projections ++= n.projections)
  }

  def where(predicate: Predicate): Select = {
    this.predicate = predicate
    this
  }

  def resultSet[A](actions: ResultSet => A): A =
    auto(configuration.connectionProvider.getConnection.prepareStatement(toSql))(st => {
      sqlLog.debug(toSql)
      (0 until predicate.parameters.size).foreach(ix =>
          configuration.typeConverter.write(st, predicate.parameters(ix), ix + 1))
      auto(st.executeQuery)(actions)
    })


  def list: Seq[Array[Any]] = resultSet(rs => {
    val result = new ListBuffer[Array[Any]]()
    while (rs.next) {
      val tuple = projections.map(_.read(rs).getOrElse(null))
      result += tuple.toArray
    }
    return result
  })

  def uniqueResult: Option[Array[Any]] = resultSet(rs => {
    if (!rs.next) return None
    else if (rs.isLast) return Some(projections.map(_.read(rs).getOrElse(null)).toArray)
    else throw new ORMException("Unique result expected, but multiple rows found.")
  })

  def firstResult: Option[Array[Any]] = resultSet(rs => {
    if (!rs.next) return None
    else return Some(projections.map(_.read(rs).getOrElse(null)).toArray)
  })

  def toSql = configuration.dialect.select(this)

  override def toString = toSql
}