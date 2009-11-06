package ru.circumflex.orm

/**
 * Result set projection.
 */
abstract class Projection[T](val alias: String,
                             val node: RelationNode[_])
    extends Configurable {

  /**
   * SQL representation of this projection for use in SELECT clause.
   */
  def toSqlSelect: String

  override def toString = toSqlSelect

}

class ColumnProjection[T](alias: String,
                          node: RelationNode[_],
                          val column: Column[T, _])
    extends Projection[T](alias, node) {

  def this(node: RelationNode[_], column: Column[T, _]) =
    this(node.alias + "_" + column.columnName, node, column)

  def toSqlSelect =
    configuration.dialect.columnAlias(column, alias, node.alias)

}