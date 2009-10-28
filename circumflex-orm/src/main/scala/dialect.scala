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

  implicit def strToEx(str: String): StringEx = new StringEx(str)
  implicit def exToStr(ex: StringEx): String = ex.toString

  /* GENERATED NAMES */

  /**
   * Produces qualified name of a table
   * (e.g. "myschema.mytable")
   */
  def tableQualifiedName(tab: Table[_]): String =
    tab.schemaName.append(".").append(tab.tableName)

  /**
   * Produces PK name (e.g. mytable_pkey).
   */
  def primaryKeyName(pk: PrimaryKey): String =
    tableQualifiedName(pk.table).append("_pkey")

  /* DEFINITIONS */

  /**
   * Produces SQL definition for a column
   * (e.g. "mycolumn varchar not null unique").
   */
  def columnDefinition(col: Column[_]): String =
    new StringEx("{0} {1} {2} {3}",
      col.name,
      col.sqlType,
      if (!col.nullable) "NOT NULL" else "",
      if (col.unique) "UNIQUE" else "")

  /**
   * Produces PK definition (e.g. "primary key(id)").
   */
  def primaryKeyDefinition(pk: PrimaryKey): String =
    new StringEx("primary key({0})", pk.columns.map(_.columnName).mkString(","))

  /* ALTER TABLE */

  /**
   * Produces ALTER TABLE statement with abstract action.
   */
  def alterTable(tab: Table[_], action: String): String =
    "alter table ".append(tableQualifiedName(tab)).append(" {0}", action)

  /**
   * Produces ALTER TABLE statement with ADD CONSTRAINT action.
   */
  def alterTableAddConstraint(tab: Table[_], constraintName: String, constaintDefinition: String): String =
    alterTable(tab, "add constraint ".append(constraintName).append(" {0}", constaintDefinition));

  /**
   * Produces ALTER TABLE statement with DROP CONSTRAINT action.
   */
  def alterTableDropConstraint(tab: Table[_], constraintName: String): String =
    alterTable(tab, "drop constraint ".append(constraintName));


}

