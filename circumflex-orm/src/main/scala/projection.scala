package ru.circumflex.orm


import java.sql.ResultSet

/**
 * Result set projection.
 */
abstract class Projection[T](val alias: String) {

  /**
   * SQL representation of this projection for use in SELECT clause.
   */
  def toSql: String

  /**
   * Extract a value from result set.
   */
  def read(rs: ResultSet): Option[T]

  override def toString = toSql

}

class ColumnProjection[T](alias: String,
                          val node: RelationNode[_],
                          val column: Column[T, _])
    extends Projection[T](alias) {

  def this(node: RelationNode[_], column: Column[T, _]) =
    this(node.alias + "_" + column.columnName, node, column)

  def toSql = node.configuration.dialect.columnAlias(column, alias, node.alias)

  def read(rs: ResultSet) = column.read(rs, alias)
}