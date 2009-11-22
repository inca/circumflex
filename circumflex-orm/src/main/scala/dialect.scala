package ru.circumflex.orm

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

  /* JOIN KEYWORDS */

  def innerJoin = "inner join"
  def leftJoin = "left join"
  def rightJoin = "right join"
  def fullJoin = "full join"

  /* SIMPLE PREDICATES AND KEYWORDS */

  def dummy = "1 = 1"
  def eq = " = ?"
  def ne = " <> ?"
  def gt = " > ?"
  def ge = " >= ?"
  def lt = " < ?"
  def le = " <= ?"
  def isNull = " is null"
  def isNotNull = " is not null"
  def like = " like ?"
  def between = " between ? and ?"
  def and = " and\n\t"
  def or = " or\n\t"
  def not = "not"

  /* ORDER SPECIFICATOR KEYWORDS */

  def asc = "asc"
  def desc = "desc"

  /* GENERATED NAMES */

  /**
   * Produces qualified name of a table
   * (e.g. "myschema.mytable").
   */
  def tableName(tab: Table) =
    tab.schemaName + "." + tab.tableName

  /**
   * Produces PK name (e.g. mytable_pkey).
   */
  def primaryKeyName(pk: PrimaryKey) =
    pk.table.tableName + "_" + pk.column.columnName + "_pkey"

  /**
   * Produces unique constraint name (e.g. mytable_name_value_key).
   */
  def uniqueKeyName(uniq: UniqueKey) =
    uniq.table.tableName + "_" + uniq.columns.map(_.columnName).mkString("_") + "_key"

  /**
   * Produces qualified sequence name (e.g. public.mytable_id_seq).
   */
  def sequenceName(seq: Sequence) =
    seq.table.tableName + "_" + seq.column.columnName + "_seq"

  /**
   * Produces foreign key constraint name (e.g. mytable_reftable_fkey).
   */
  def foreignKeyName(fk: ForeignKey[_]) =
    fk.table.tableName + "_" + fk.localColumn.columnName + "_fkey"

  /* DEFINITIONS */

  /**
   * Produces SQL definition for a column
   * (e.g. "mycolumn varchar not null unique").
   */
  def columnDefinition(col: Column[_]) =
    col.columnName + " " + col.sqlType + (if (!col.nullable) " not null" else "")

  /**
   * Produces PK definition (e.g. "primary key (id)").
   */
  def primaryKeyDefinition(pk: PrimaryKey) =
    "primary key (" + pk.column.columnName + ")"

  /**
   * Produces unique constraint definition (e.g. "unique (name, value)").
   */
  def uniqueKeyDefinition(uniq: UniqueKey) =
    "unique (" + uniq.columns.map(_.columnName).mkString(",") + ")"

  /**
   * Produces foreign key constraint definition
   * (e.g. "foreign key (ref_id) references public.ref(id) on delete cascade on update no action").
   */
  def foreignKeyDefinition(fk: ForeignKey[_]) =
    "foreign key (" + fk.localColumn.columnName + ") references " +
        tableName(fk.referenceTable) + " (" + fk.referenceColumn.columnName + ")\n\t\t" +
        "on delete " + foreignKeyAction(fk.onDelete) + "\n\t\t" + "" +
        "on update " + foreignKeyAction(fk.onUpdate)


  /**
   * Produces constraint definition (e.g. "constraint mytable_pkey primary key(id)").
   */
  def constraintDefinition(constraint: Constraint) =
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
  def createSequence(seq: Sequence) =
    "create sequence " + seq.sequenceName + "\n\tstart with 1 increment by 1"

  /**
   * Produces CREATE TABLE statement without constraints.
   */
  def createTable(tab: Table) =
    "create table " + tableName(tab) + " (\n\t" +
        tab.columns.map(_.sqlDefinition).mkString(",\n\t") + ",\n\t" +
        tab.primaryKey.sqlFullDefinition + "\n)"

  /**
   * Produces ALTER TABLE statement with abstract action.
   */
  def alterTable(tab: Table, action: String) =
    "alter table " + tableName(tab) + "\n\t" + action

  /**
   * Produces ALTER TABLE statement with ADD CONSTRAINT action.
   */
  def alterTableAddConstraint(constraint: Constraint) =
    alterTable(constraint.table, "add " + constraintDefinition(constraint));

  /**
   * Produces ALTER TABLE statement with ADD COLUMN action.
   */
  def alterTableAddColumn(column: Column[_]) =
    alterTable(column.table, "add column " + columnDefinition(column));

  /**
   * Produces ALTER TABLE statement with DROP CONSTRAINT action.
   */
  def alterTableDropConstraint(constraint: Constraint) =
    alterTable(constraint.table, "drop constraint " + constraint.constraintName);

  /**
   * Produces ALTER TABLE statement with DROP COLUMN action.
   */
  def alterTableDropColumn(column: Column[_]) =
    alterTable(column.table, "drop column " + column.columnName);

  /**
   * Produces DROP TABLE statement
   */
  def dropTable(tab: Table) =
    "drop table " + tableName(tab)

  /**
   * Produces DROP SEQUENCE statement.
   */
  def dropSequence(seq: Sequence) =
    "drop sequence " + seq.sequenceName

  /**
   * Produces DROP SCHEMA statement.
   */
  def dropSchema(schema: Schema) =
    "drop schema " + schema.schemaName

  /* SELECT STATEMENTS AND RELATED */

  /**
   * Produces a statement to select a single next sequence value.
   */
  def selectSequenceNextVal(seq: Sequence) =
    "select nextval('" + sequenceName(seq) + "')"

  def columnAlias(col: Column[_], columnAlias: String, tableAlias: String) =
    qualifyColumn(col, tableAlias) + " as " + columnAlias

  /**
   * Produces table with alias (e.g. "public.mytable my").
   */
  def tableAlias(tab: Table, alias: String) = tab.qualifiedName + " as " + alias

  /**
   * Qualifies a column with table alias (e.g. "p.id")
   */
  def qualifyColumn(col: Column[_], tableAlias: String) = tableAlias + "." + col.columnName

  /**
   * Produces join node sql representation (e.g. person p left join address a on p.id = a.person_id).
   */
  def join(j: JoinNode): String = joinInternal(j, null)

  /**
   * Some magic to convert join tree to SQL.
   */
  protected def joinInternal(node: RelationNode, on: String): String = {
    var result = ""
    node match {
      case j: JoinNode => {
        val parentAlias = if (j.isInverse) j.rightNode.alias else j.leftNode.alias
        val childAlias = if (j.isInverse) j.leftNode.alias else j.rightNode.alias
        result += joinInternal(j.leftNode, on) + "\n\t\t" + j.sqlJoinType + " " +
            joinInternal(j.rightNode, joinOn(j.association, parentAlias, childAlias))
      } case _ => {
        result += node.toSql
        if (on != null) result += "\n\t\t\t" + on
      }
    }
    return result
  }

  // ON subclause for joins (e.g. "on (c.id = b.category_id)")
  protected def joinOn(association: Association,
                       parentAlias: String,
                       childAlias: String) =
    "on (" + qualifyColumn(association.referenceColumn, parentAlias) + " = " +
        qualifyColumn(association.localColumn, childAlias) + ")"

  /**
   * Formats provided projections for use in SELECT clause (just comma-delimited mkString).
   */
  def selectClause(projections: String *) = projections.mkString(",\n\t")

  /**
   * Produces SELECT statement.
   */
  def select(q: Select): String = {
    var result = "select\n\t" + q.projections.map(_.toSql).mkString(",\n\t") +
        "\nfrom\n\t" + q.relations.map(_.toSql).mkString(",\n\t")
    if (q.where != EmptyPredicate) result += "\nwhere\n\t" + q.where.toSql
    if (q.orders.size > 0)
      result += "\norder by\n\t" + q.orders.map(_.expression).mkString(",\n\t")
    if (q.limit > -1)
      result += "\nlimit " + q.limit
    if (q.offset > 0)
      result += "\noffset " + q.offset
    return result
  }

  /**
   * Produces SQL for ascending order.
   */
  def orderAsc(expr: String) = expr + " " + asc

  /**
   * Produces SQL for descending order.
   */
  def orderDesc(expr: String) = expr + " " + desc

  /* INSERT STATEMENTS */

  /**
   * Produces INSERT INTO .. VALUES statement.
   */
  def insertRecord(record: Record): String =
    "insert into " + record.relation.qualifiedName +
        " (\n\t" + record.relation.columns.map(_.columnName).mkString(",\n\t") +
        ") values (" + record.relation.columns.map(_ => "?").mkString(", ") + ")"

  /* UPDATE STATEMENTS */

  /**
   * Produces UPDATE statement with primary key criteria.
   */
  def updateRecord(record: Record): String =
    "update " + record.relation.qualifiedName +
        "\nset\n\t" + record.relation.nonPKColumns.map(_.columnName + " = ?").mkString(",\n\t") +
        "\nwhere\n\t" + record.relation.primaryKey.column.columnName + " = ?"

  /* DELETE STATEMENTS */

  /**
   * Produces DELETE statement with primary key criteria.
   */
  def deleteRecord(record: Record): String =
    "delete from " + record.relation.qualifiedName +
        "\nwhere\n\t" + record.relation.primaryKey.column.columnName + " = ?"

}