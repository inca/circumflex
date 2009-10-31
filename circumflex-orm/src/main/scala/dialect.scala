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

/**
 * A default dialect singleton.
 * If you feel that some of the statements do not work
 * with your RDBMS vendor, trace the exact method and provide it's
 * implementation in your object. Of course, you should override the
 * default configuration with your dialect object in that case.
 */
object DefaultDialect extends Dialect

/**
 * This little thingy does all dirty SQL stuff.
 */
trait Dialect {

  /* SQL TYPES */

  def longType = "int8"
  def stringType = "text"

  /* FOREIGN KEY ACTIONS */
  def foreignKeyAction(action: ForeignKeyAction) = action match {
    case NoAction => "no action"
    case CascadeAction => "cascade"
    case RestrictAction => "restrict"
    case SetNullAction => "set null"
    case SetDefaultAction => "set default"
  }

  /* GENERATED NAMES */

  /**
   * Produces qualified name of a table
   * (e.g. "myschema.mytable")
   */
  def tableName(tab: Table[_]) =
    tab.schemaName + "." + tab.tableName

  /**
   * Used to group common parts for constraint names (concatenate table and column names,
   * separated by underscore).
   */
  protected def constraintBaseName(constr: Constraint[_]) =
    constr.table.tableName + "_" + constr.columns.map(_.columnName).mkString("_")

  /**
   * Produces PK name (e.g. mytable_pkey).
   */
  def primaryKeyName(pk: PrimaryKey[_]) =
    constraintBaseName(pk) + "_pkey"

  /**
   * Produces unique constraint name (e.g. mytable_name_value_key).
   */
  def uniqueKeyName(uniq: UniqueKey[_]) =
    constraintBaseName(uniq) + "_key"

  /**
   * Produces qualified sequence name (e.g. public.mytable_id_seq).
   */
  def sequenceName(seq: Sequence[_, _]) =
    seq.table.tableName + "_" + seq.column.columnName + "_seq"

  /**
   * Produces foreign key constraint name (e.g. mytable_reftable_fkey).
   */
  def foreignKeyName(fk: ForeignKey[_, _]) =
    constraintBaseName(fk) + "_fkey"

  /* DEFINITIONS */

  /**
   * Produces SQL definition for a column
   * (e.g. "mycolumn varchar not null unique").
   */
  def columnDefinition(col: Column[_, _]) =
    col.columnName + " " + col.sqlType + (if (!col.nullable) " not null" else "")

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
   * Produces foreign key constraint definition
   * (e.g. "foreign key (ref_id) references public.ref(id) on delete cascade on update no action").
   */
  def foreignKeyDefinition(fk: ForeignKey[_, _]) =
    "foreign key (" + fk.columns.map(_.columnName).mkString(",") +
        ") references " + tableName(fk.referenceTable) + " (" +
        fk.referenceTable.primaryKey.columns.map(_.columnName).mkString(",") + ")\n\t\t" +
        "on delete " + foreignKeyAction(fk.onDelete) + "\n\t\t" + "" +
        "on update " + foreignKeyAction(fk.onUpdate)


  /**
   * Produces constraint definition (e.g. "constraint mytable_pkey primary key(id)").
   */
  def constraintDefinition(constraint: Constraint[_]) =
    "constraint " + constraint.constraintName + "\n\t\t" + constraint.sqlDefinition

  /* CREATE/ALTER/DROP STATEMENTS */

  /**
   * Produces CREATE SCHEMA statement.
   */
  def createSchema(schema: Schema) =
    "create schema " + schema.schemaName

  /**
   * Produces CREATE SEQUENCE statement.
   */
  def createSequence(seq: Sequence[_, _]) =
    "create sequence " + sequenceName(seq) + "\n\tstart with 1 increment by 1"

  /**
   * Produces CREATE TABLE statement without constraints.
   */
  def createTable(tab: Table[_]) =
    "create table " + tableName(tab) + " (\n\t" +
        tab.columns.map(_.sqlDefinition).mkString(",\n\t") + ",\n\t" +
        tab.primaryKey.sqlFullDefinition + "\n)"

  /**
   * Produces ALTER TABLE statement with abstract action.
   */
  def alterTable(tab: Table[_], action: String) =
    "alter table " + tableName(tab) + "\n\t" + action

  /**
   * Produces ALTER TABLE statement with ADD CONSTRAINT action.
   */
  def alterTableAddConstraint(constraint: Constraint[_]) =
    alterTable(constraint.table, "add " + constraintDefinition(constraint));

  /**
   * Produces ALTER TABLE statement with ADD COLUMN action.
   */
  def alterTableAddColumn(column: Column[_, _]) =
    alterTable(column.table, "add column " + columnDefinition(column));

  /**
   * Produces ALTER TABLE statement with DROP CONSTRAINT action.
   */
  def alterTableDropConstraint(constraint: Constraint[_]) =
    alterTable(constraint.table, "drop constraint " + constraint.constraintName);

  /**
   * Produces ALTER TABLE statement with DROP COLUMN action.
   */
  def alterTableDropColumn(column: Column[_, _]) =
    alterTable(column.table, "drop column " + column.columnName);

  /**
   * Produces DROP TABLE statement
   */
  def dropTable(tab: Table[_]) =
    "drop table " + tableName(tab)

  /**
   * Produces DROP SEQUENCE statement.
   */
  def dropSequence(seq: Sequence[_, _]) =
    "drop sequence " + sequenceName(seq)

  /**
   * Produces DROP SCHEMA statement.
   */
  def dropSchema(schema: Schema) =
    "drop schema " + schema.schemaName
  
  /* SELECT STATEMENTS */

  /**
   * Produces a statement to select a single next sequence value.
   */
  def selectSequenceNextVal(seq: Sequence[_, _]) =
    "select nextval('" + sequenceName(seq) + "')"

}

