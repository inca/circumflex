package ru.circumflex.orm

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

  /* SQL TYPES */

  def longType = "int8"
  def stringType = "text"

  /* GENERATED NAMES */

  /**
   * Produces qualified name of a table
   * (e.g. "myschema.mytable")
   */
  def tableQualifiedName(tab: Table[_]) =
    tab.schemaName + "." + tab.tableName

  /**
   * Produces PK name (e.g. mytable_pkey).
   */
  def primaryKeyName(pk: PrimaryKey[_]) =
    pk.table.tableName + "_pkey"

  /**
   * Produces unique constraint name (e.g. mytable_name_value_key).
   */
  def uniqueKeyName(uniq: UniqueKey[_]) =
    uniq.table.tableName + "_" + uniq.columns.map(_.columnName).mkString("_") + "_key"

  /* DEFINITIONS */

  /**
   * Produces SQL definition for a column
   * (e.g. "mycolumn varchar not null unique").
   */
  def columnDefinition(col: Column[_, _]) =
    col.columnName + " " + col.sqlType + (if (!col.isNullable) " not null" else "")

  /**
   * Produces PK definition (e.g. "primary key (id)").
   */
  def primaryKeyDefinition(pk: PrimaryKey[_]) =
    "primary key (" + pk.columns.map(_.columnName).mkString(",") + ")"

  /**
   * Produces unique constraint definition (e.g. "unique (name, value)").
   */
  def uniqueKeyDefinition(uniq: UniqueKey[_]) =
    "unique (" + uniq.columns.map(_.columnName).mkString(",") + ")"

  /**
   * Produces constraint definition (e.g. "constraint mytable_pkey primary key(id)").
   */
  def constraintDefinition(constraint: Constraint[_]) =
    "constraint " + constraint.constraintName + " " + constraint.sqlDefinition

  /* CREATE TABLE */
  def createTable(tab: Table[_]): String = {
    var buf = "create table " + tableQualifiedName(tab) + " (\n\t" +
        tab.columns.map(_.sqlDefinition).mkString(",\n\t") + ",\n\t" +
        tab.primaryKey.sqlFullDefinition
    if (tab.constraints.size > 0)
      buf += ",\n\t" + tab.constraints.map(_.sqlFullDefinition).mkString(",\n\t")
    return buf + ")"
  }

  /* ALTER TABLE */

  /**
   * Produces ALTER TABLE statement with abstract action.
   */
  def alterTable(tab: Table[_], action: String) =
    "alter table " + tableQualifiedName(tab) + "\n\t" + action

  /**
   * Produces ALTER TABLE statement with ADD CONSTRAINT action.
   */
  def alterTableAddConstraint(tab: Table[_], constraint: Constraint[_]) =
    alterTable(tab, "add " + constraintDefinition(constraint));

  /**
   * Produces ALTER TABLE statement with DROP CONSTRAINT action.
   */
  def alterTableDropConstraint(tab: Table[_], constraint: Constraint[_]) =
    alterTable(tab, "drop constraint " + constraint.constraintName);


}

