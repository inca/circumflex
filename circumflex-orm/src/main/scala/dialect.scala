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

  /* GENERATED NAMES */

  /**
   * Produces qualified name of a table
   * (e.g. "myschema.mytable")
   */
  def tableQualifiedName(tab: Table[_]): String =
    tab.schemaName + "." + tab.tableName

  /**
   * Produces PK name (e.g. mytable_pkey).
   */
  def primaryKeyName(pk: PrimaryKey[_]): String =
    tableQualifiedName(pk.table) + "_pkey"

  /* DEFINITIONS */

  /**
   * Produces SQL definition for a column
   * (e.g. "mycolumn varchar not null unique").
   */
  def columnDefinition(col: Column[_, _]): String =
      col.columnName + " " + col.sqlType + (if (!col.isNullable) " NOT NULL" else "")

  /**
   * Produces PK definition (e.g. "primary key(id)").
   */
  def primaryKeyDefinition(pk: PrimaryKey[_]): String =
    "primary key(" + pk.columns.map(_.columnName).mkString(",") + ")"

  /* ALTER TABLE */

  /**
   * Produces ALTER TABLE statement with abstract action.
   */
  def alterTable(tab: Table[_], action: String): String =
    "alter table " + tableQualifiedName(tab) + " " + action

  /**
   * Produces ALTER TABLE statement with ADD CONSTRAINT action.
   */
  def alterTableAddConstraint(tab: Table[_], constraintName: String, constaintDefinition: String): String =
    alterTable(tab, "add constraint " + constraintName  + " " +constaintDefinition);

  /**
   * Produces ALTER TABLE statement with DROP CONSTRAINT action.
   */
  def alterTableDropConstraint(tab: Table[_], constraintName: String): String =
    alterTable(tab, "drop constraint " + constraintName);


}

