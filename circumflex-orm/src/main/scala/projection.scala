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

/**
 * Defines a contract for projections with assigned aliases
 * so that they may be used in ORDER BY/HAVING clauses.
 */
trait AliasedProjection[T] extends Projection[T] {
  def alias: String
}

class FieldProjection[T, R](val alias: String,
                            val node: RelationNode[R],
                            val column: Column[T, R])
    extends AliasedProjection[T] {

  def this(node: RelationNode[R], column: Column[T, R]) =
    this (node.alias + "_" + column.columnName, node, column)

  def read(rs: ResultSet): Option[T] =
    node.typeConverter.read(rs, alias).asInstanceOf[Option[T]]

  /**
   * Change an alias of this projection.
   */
  def as(alias: String) = new FieldProjection(alias, node, column)

  /**
   * Returns a column name qualified with node's alias.
   */
  def expr = node.dialect.qualifyColumn(column, node.alias)

  def toSql = node.dialect.columnAlias(column, alias, node.alias)

}

class RecordProjection[R](val node: RelationNode[R])
    extends Projection[R] {
  val columnProjections: Seq[FieldProjection[Any, R]] =
  node.relation.columns.map(col => new FieldProjection(node, col.asInstanceOf[Column[Any, R]]))

  def read(rs: ResultSet): Option[R] = {
    val record = node.relation.recordClass
        .getConstructor()
        .newInstance()
        .asInstanceOf[Record[R]]
    columnProjections.foreach(
      p => record.setField(p.column, p.read(rs)))
    if (record.isIdentified) return Some(record.asInstanceOf[R])
    else return None
  }

  def toSql = node.dialect.selectClause(columnProjections.map(_.toSql): _*)
}