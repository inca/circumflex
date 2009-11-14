package ru.circumflex.orm


import java.sql.ResultSet

/**
 * Result set projection.
 */
trait Projection[T] {

  /**
   * Extract a value from result set.
   */
  def read(rs: ResultSet): Option[T]

  /**
   * SQL representation of this projection for use in SELECT clause.
   */
  def toSql: String

  override def toString = toSql
}

class ColumnProjection[T](alias: String,
                          val node: RelationNode[_],
                          val column: Column[T, _])
    extends Projection[T] {

  def this(node: RelationNode[_], column: Column[T, _]) =
    this(node.alias + "_" + column.columnName, node, column)

  def read(rs: ResultSet): Option[T] =
    column.read(rs, alias)

  def toSql = node.configuration.dialect.columnAlias(column, alias, node.alias)
}

class RecordProjection[R <: Record](val tableNode: TableNode[R])
    extends Projection[R] {

  val columnProjections: Seq[ColumnProjection[_]] =
    tableNode.table.columns.map(col => new ColumnProjection(tableNode, col))

  def read(rs: ResultSet): Option[R] = {
    val record = tableNode.table.recordClass
        .getConstructor()
        .newInstance()
    columnProjections.foreach(p => record.update(p.column.asInstanceOf[Column[Any, _]], p.read(rs)))
    if (record.isIdentified) return Some(record)
    else return None 
  }

  def toSql = tableNode.configuration.dialect.selectClause(columnProjections.map(_.toSql): _*)
}