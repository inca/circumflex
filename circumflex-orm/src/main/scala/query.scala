package ru.circumflex.orm


import collection.mutable.ListBuffer
import java.sql.{ResultSet, PreparedStatement}
import org.slf4j.LoggerFactory

class Query(override val configuration: Configuration)
    extends Configurable
        with JDBCHelper {

  val log = LoggerFactory.getLogger("ru.circumflex.orm.SQL")

  var projections: Seq[Projection[_]] = Nil
  var relations: Seq[RelationNode[_]] = Nil

  def from(nodes: RelationNode[_ <: Record] *): Query = {
    relations ++= nodes.toList
    nodes.foreach(n => projections ++= n.projections)
    return this
  }

  def resultSet[A](actions: ResultSet => A): A =
    auto(configuration.connectionProvider.getConnection) {
      conn => auto(conn.prepareStatement(toSql)) {
        st => {
          log.debug(toSql)
          auto(st.executeQuery)(actions)
        }
      }
    }

  def list: Seq[Array[Any]] = resultSet(rs => {
    val result = new ListBuffer[Array[Any]]()
    while (rs.next) {
      val tuple = projections.map(_.read(rs).getOrElse(null))
      result += tuple.toArray
    }
    return result
  })

  def toSql = configuration.dialect.select(this)

  override def toString = toSql
}