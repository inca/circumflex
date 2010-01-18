/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

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
class Dialect {

  /* SQL TYPES */

  def longType = "int8"
  def stringType = "text"
  def booleanType = "boolean"
  def timestampType = "timestamptz"

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

  def parameterizedIn(params: Seq[_]) =
    " in (" + params.map(p => "?").mkString(", ") + ")"

  def subquery(expr: String, subselect: Subselect) =
    expr + " (\n" + subselect.toSubselectSql + "\n)"

  /* ORDER SPECIFICATOR KEYWORDS */

  def asc = "asc"

  def desc = "desc"

  /* COMMON STUFF */

  def quoteLiteral(expr: String) = "'" + expr.replace("'", "''") + "'"

  /**
   * Produces qualified name of a table
   * (e.g. "myschema.mytable").
   */
  def qualifyRelation(rel: Relation[_]) =
    rel.schemaName + "." + rel.relationName

  def qualifyColumn(col: Column[_, _]) =
    col.relation.relationName + "." + col.columnName

  /**
   * Produces qualified sequence name (e.g. public.mytable_id_seq).
   */
  def sequenceName(seq: Sequence[_]) =
    qualifyRelation(seq.relation) + "_" + seq.column.columnName + "_seq"

  /**
   * Override this definition to provide logical "unwrapping" for relation-based operations.
   */
  protected def unwrap(relation: Relation[_]) = relation

  /* DEFINITIONS */

  /**
   * Produces SQL definition for a column
   * (e.g. "mycolumn varchar not null unique").
   */
  def columnDefinition(col: Column[_, _]) =
    col.columnName + " " + col.sqlType + (if (!col.nullable_?) " not null" else "")

  /**
   * Produces PK definition (e.g. "primary key (id)").
   */
  def primaryKeyDefinition(pk: PrimaryKey[_, _]) =
    "primary key (" + pk.column.columnName + ")"

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
    "foreign key (" + fk.childColumns.map(_.columnName).mkString(", ") +
        ") references " + unwrap(fk.parentRelation).qualifiedName + " (" +
        fk.parentColumns.map(_.columnName).mkString(", ") + ")\n\t\t" +
        "on delete " + foreignKeyAction(fk.onDelete) + "\n\t\t" + "" +
        "on update " + foreignKeyAction(fk.onUpdate)

  /**
   * Produces check constraint definition (e.g. "check (age between 18 and 40)").
   */
  def checkConstraintDefinition(check: CheckConstraint[_]) =
    "check (" + check.expression + ")"

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
  def createSequence(seq: Sequence[_]) =
    "create sequence " + seq.sequenceName + "\n\tstart with 1 increment by 1"

  /**
   * Produces CREATE TABLE statement without constraints.
   */
  def createTable(tab: Table[_]) =
    "create table " + qualifyRelation(tab) + " (\n\t" +
        tab.columns.map(_.sqlDefinition).mkString(",\n\t") + ",\n\t" +
        tab.primaryKey.sqlFullDefinition + "\n)"

  /**
   * Produces CREATE VIEW statement.
   */
  def createView(view: View[_]) =
    "create view " + qualifyRelation(view) + " (\n\t" +
        view.columns.map(_.columnName).mkString(",\n\t") + ")\nas " +
        view.query.toInlineSql

  /**
   * Produces CREATE INDEX statement.
   */
  def createIndex(index: Index[_]): String =
    "create " + (if (index.unique_?) "unique" else "") + " index " +
        index.indexName + "\n\ton " + unwrap(index.relation).qualifiedName + " using " +
        index.using + " (" + index.expressions.mkString(", ") + ")" +
        (if (index.where != EmptyPredicate)
          "\n\twhere\n\t" + index.where.toInlineSql
        else "")

  /**
   * Produces ALTER TABLE statement with abstract action.
   */
  def alterTable(rel: Relation[_], action: String) =
    "alter table " + unwrap(rel).qualifiedName + "\n\t" + action

  /**
   * Produces ALTER TABLE statement with ADD CONSTRAINT action.
   */
  def alterTableAddConstraint(constraint: Constraint[_]) =
    alterTable(constraint.relation, "add " + constraintDefinition(constraint));

  /**
   * Produces ALTER TABLE statement with ADD COLUMN action.
   */
  def alterTableAddColumn(column: Column[_, _]) =
    alterTable(column.relation, "add column " + columnDefinition(column));

  /**
   * Produces ALTER TABLE statement with DROP CONSTRAINT action.
   */
  def alterTableDropConstraint(constraint: Constraint[_]) =
    alterTable(constraint.relation, "drop constraint " + constraint.constraintName);

  /**
   * Produces ALTER TABLE statement with DROP COLUMN action.
   */
  def alterTableDropColumn(column: Column[_, _]) =
    alterTable(column.relation, "drop column " + column.columnName);

  /**
   * Produces DROP TABLE statement
   */
  def dropTable(tab: Table[_]) =
    "drop table " + qualifyRelation(tab)

  /**
   * Produces DROP VIEW statement
   */
  def dropView(view: View[_]) =
    "drop view " + qualifyRelation(view)

  /**
   * Produces DROP SEQUENCE statement.
   */
  def dropSequence(seq: Sequence[_]) =
    "drop sequence " + seq.sequenceName

  /**
   * Produces DROP SCHEMA statement.
   */
  def dropSchema(schema: Schema) =
    "drop schema " + schema.schemaName + " cascade"

  /**
   * Produces DROP INDEX statement.
   */
  def dropIndex(index: Index[_]) =
    "drop index " + index.relation.schemaName + "." + index.indexName

  /* SEQUENCES STUFF */

  def sequenceCurrVal(seq: Sequence[_]): String = "currval('" + sequenceName(seq) + "')"
  def sequenceCurrVal(seq: Sequence[_], alias: String): String =
    sequenceCurrVal(seq) + " as " + alias

  def sequenceNextVal(seq: Sequence[_]): String = "nextval('" + sequenceName(seq) + "')"
  def sequenceNextVal(seq: Sequence[_], alias: String): String =
    sequenceNextVal(seq) + " as " + alias

  /* SELECT STATEMENTS AND RELATED */

  /**
   * Produces a statement to select a single next sequence value.
   */
  def selectSequenceNextVal(seq: Sequence[_]) =
    "select " + sequenceNextVal(seq)

  def columnAlias(col: Column[_, _], columnAlias: String, tableAlias: String) =
    qualifyColumn(col, tableAlias) + " as " + columnAlias

  def scalarAlias(expression: String, alias: String) = expression + " as " + alias

  /**
   * Produces table with alias (e.g. "public.mytable my").
   */
  def tableAlias(tab: Table[_], alias: String) = tab.qualifiedName + " as " + alias

  /**
   * Produces table with alias (e.g. "public.mytable my").
   */
  def viewAlias(view: View[_], alias: String) = view.qualifiedName + " as " + alias

  /**
   * Qualifies a column with table alias (e.g. "p.id")
   */
  def qualifyColumn(col: Column[_, _], tableAlias: String) = tableAlias + "." + col.columnName

  /**
   * Produces join node sql representation (e.g. person p left join address a on p.id = a.person_id).
   */
  def join(j: JoinNode[_, _]): String = joinInternal(j, null)

  /**
   * Some magic to convert join tree to SQL.
   */
  protected def joinInternal(node: RelationNode[_], on: String): String = {
    var result = ""
    node match {
      case j: ChildToParentJoin[_, _] =>
        result += joinInternal(j.left, on) + "\n\t\t" + j.sqlJoinType + " " +
            joinInternal(j.right, joinOn(
              j.association,
              j.right.alias,
              j.left.alias,
              j.auxiliaryConditions))
      case j: ParentToChildJoin[_, _] =>
        result += joinInternal(j.left, on) + "\n\t\t" + j.sqlJoinType + " " +
            joinInternal(j.right, joinOn(
              j.association,
              j.left.alias,
              j.right.alias,
              j.auxiliaryConditions))
      case _ =>
        result += node.toSql
        if (on != null) result += "\n\t\t\t" + on
    }
    return result
  }

  // ON subclause for joins (e.g. "on (c.id = b.category_id)")
  protected def joinOn(association: Association[_, _],
                       parentAlias: String,
                       childAlias: String,
                       auxConditions: Seq[String]): String = {
    var result =  "on (" + qualifyColumn(association.parentColumn, parentAlias) +
        " = " + qualifyColumn(association.childColumn, childAlias)
    if (auxConditions.size > 0)
      result += "\n\t\t\t\tand " + auxConditions.mkString("\n\t\t\t\tand ")
    result += ")"
    return result
  }

  /**
   * Formats provided projections for use in SELECT clause (just comma-delimited mkString).
   */
  def selectClause(projections: String*) = projections.mkString(",\n\t")

  /**
   * Produces SELECT statement with ? parameters.
   */
  def select(q: Select): String = {
    var result = subselect(q)
    if (q.orders.size > 0)
      result += "\norder by\n\t" + q.orders.map(_.expression).mkString(",\n\t")
    if (q.limit > -1)
      result += "\nlimit " + q.limit
    if (q.offset > 0)
      result += "\noffset " + q.offset
    return result
  }

  /**
   * Produces SELECT statement (without LIMIT, OFFSET and ORDER BY clauses).
   */
  def subselect(q: Subselect): String = {
    var result = "select\n\t" + q.projections.map(_.toSql).mkString(",\n\t")
    if (q.relations.size > 0)
      result += "\nfrom\n\t" + q.relations.map(_.toSql).mkString(",\n\t")
    if (q.where != EmptyPredicate)
      result += "\nwhere\n\t" + q.where.toSql
    if (q.groupBy.size > 0)
      result += "\ngroup by\n\t" + q.groupBy.flatMap(_.sqlAliases).mkString(",\n\t")
    if (q.having != EmptyPredicate)
      result += "\nhaving\n\t" + q.having.toSql
    q.setOps.foreach {
      case (op: SetOperation, subq: Subselect) =>
        result += "\n" + op.expression + " (" + subq.toSubselectSql + "\n)"
      case _ =>
    }
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
  def insertRecord(record: Record[_]): String =
    "insert into " + record.relation.qualifiedName +
        " (\n\t" + record.relation.columns.map(_.columnName).mkString(",\n\t") +
        ") values (" + record.relation.columns.map(_ => "?").mkString(", ") + ")"

  /**
   * Produces INSERT INTO .. SELECT statement.
   */
  def insertSelect(dml: InsertSelect[_]): String =
    "insert into " + dml.relation.qualifiedName +
        " (\n\t" + dml.relation.columns.map(_.columnName).mkString(",\n\t") +
        ") " + select(dml.query)

  /* UPDATE STATEMENTS */

  /**
   * Produces UPDATE statement with primary key criteria.
   */
  def updateRecord(record: Record[_]): String =
    "update " + record.relation.qualifiedName +
        "\nset\n\t" + record.relation.nonPKColumns.map(_.columnName + " = ?").mkString(",\n\t") +
        "\nwhere\n\t" + record.relation.primaryKey.column.columnName + " = ?"

  /**
   * Produces UPDATE statement.
   */
  def update(dml: Update[_]): String = {
    var result = "update " + dml.relation.qualifiedName +
        "\nset\n\t" + dml.setClause.map(_._1.columnName + " = ?").mkString(",\n\t")
    if (dml.where != EmptyPredicate) result += "\nwhere\n\t" + dml.where.toSql
    return result
  }

  /* DELETE STATEMENTS */

  /**
   * Produces DELETE statement with primary key criteria.
   */
  def deleteRecord(record: Record[_]): String =
    "delete from " + record.relation.qualifiedName +
        "\nwhere\n\t" + record.relation.primaryKey.column.columnName + " = ?"

  /**
   * Produces DELETE statement.
   */
  def delete(dml: Delete[_]): String = {
    var result = "delete from " + dml.relation.qualifiedName
    if (dml.where != EmptyPredicate) result += "\nwhere\n\t" + dml.where.toSql
    return result
  }


}