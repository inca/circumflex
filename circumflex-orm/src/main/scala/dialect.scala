package ru.circumflex.orm

import util._

object DialectInterpolations {
  val selectClause = "{select_clause}"
  val fromClause = "{from_clause}"
  val whereClause = "{where_clause}"
  val groupByClause = "{group_by_clause}"
  val havingClause = "{having_clause}"
  val orderByClause = "{order_by_clause}"
  val limitClause = "{limit_clause}"
  val offsetClause = "{offset_clause}"
  val distinctClause = "{distinct_clause}"
  val setClause = "{set_clause}"
  val valuesClause = "{values_clause}"
}

object DefaultDialect extends Dialect

trait Dialect {

  /**
   * Produces SQL definition for a column (e.q. "mycolumn varchar not null unique").
   * @return SQL column definition
   */
  def columnDefinition(col: Column[_]) = new StringBuilderEx("{0} {1} {2} {3}",
    col.name,
    col.sqlType,
    if (!col.nullable) "NOT NULL" else "",
    if (col.unique) "UNIQUE" else "")
      .toString

}

