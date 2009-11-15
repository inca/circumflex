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

class FieldProjection[T](val alias: String,
                         val node: RelationNode,
                         val column: Column[T])
    extends AliasedProjection[T] with Configurable {

  override def configuration = node.configuration

  def this(node: RelationNode, column: Column[T]) =
    this(node.alias + "_" + column.columnName, node, column)

  def read(rs: ResultSet): Option[T] =
    node.configuration.typeConverter.read(rs, alias).asInstanceOf[Option[T]]

  /**
   * Change an alias of this projection.
   */
  def as(alias: String) = new FieldProjection(alias, node, column)

  /**
   * Returns a column name qualified with node's alias.
   */
  def expr = dialect.qualifyColumn(column, node.alias)

  def toSql = dialect.columnAlias(column, alias, node.alias)

}

class RecordProjection[R <: Record](val tableNode: TableNode)
    extends Projection[R] {

  val columnProjections: Seq[FieldProjection[_]] =
  tableNode.table.columns.map(col => new FieldProjection(tableNode, col))

  def read(rs: ResultSet): Option[R] = {
    val record = tableNode.table.recordClass
        .getConstructor()
        .newInstance()
        .asInstanceOf[R]
    columnProjections.foreach(p => record.update(p.column.asInstanceOf[Column[Any]], p.read(rs)))
    if (record.isIdentified) return Some(record)
    else return None
  }

  def toSql = tableNode.configuration.dialect.selectClause(columnProjections.map(_.toSql): _*)
}