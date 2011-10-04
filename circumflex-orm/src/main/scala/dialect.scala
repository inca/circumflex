package ru.circumflex
package orm

import java.sql._

/*!# Dialect

This little thingy does all dirty SQL rendering.

We are orienting the default dialect on the world's most advanced open-source
database, [PostgreSQL][psql].

If you feel that some of the statements do not work with your RDBMS vendor,
trace the exact method and provide it's implementation in your own class.
After that, set the `orm.dialect` configuration parameter accordingly.

  [psql]: http://postgresql.org
*/
class Dialect {

  def driverClass: String =
    throw new ORMException("Missing mandatory configuration parameter 'orm.connection.driver'.")

  /*!## JDBC methods */

  def prepareStatement(conn: Connection, sql: String): PreparedStatement =
    conn.prepareStatement(sql)

  /*!## SQL types */

  def longType = "BIGINT"
  def integerType = "INTEGER"
  def numericType(precision: Int, scale: Int): String =
    "NUMERIC" + (if (precision == -1) "" else "(" + precision + "," + scale + ")")
  def textType = "TEXT"
  def varcharType(length: Int): String =
    "VARCHAR" + (if (length == -1) "" else "(" + length + ")")
  def booleanType = "BOOLEAN"
  def dateType = "DATE"
  def timeType = "TIME"
  def timestampType = "TIMESTAMP"
  def xmlType = "XML"
  def binaryType = "BYTEA"

  /*!## Actions for Foreign Keys */

  def fkNoAction = "NO ACTION"
  def fkCascade = "CASCADE"
  def fkRestrict = "RESTRICT"
  def fkSetNull = "SET NULL"
  def fkSetDefault = "SET DEFAULT"

  /*!## Join Keywords */

  def innerJoin = "INNER JOIN"
  def leftJoin = "LEFT JOIN"
  def rightJoin = "RIGHT JOIN"
  def fullJoin = "FULL JOIN"

  /*!## Predicates */

  def EQ(ex1: String, ex2: String = "?") = ex1 + " = " + ex2
  def NE(ex1: String, ex2: String = "?") = ex1 + " <> " + ex2
  def GT(ex1: String, ex2: String = "?") = ex1 + " > " + ex2
  def GE(ex1: String, ex2: String = "?") = ex1 + " >= " + ex2
  def LT(ex1: String, ex2: String = "?") = ex1 + " < " + ex2
  def LE(ex1: String, ex2: String = "?") = ex1 + " <= " + ex2

  def emptyPredicate = "1 = 1"
  def IS_NULL(ex: String) = ex + " IS NULL"
  def IS_NOT_NULL(ex: String) = ex + " IS NOT NULL"
  def LIKE(ex1: String, ex2: String = "?") = ex1 + " LIKE " + ex2
  def ILIKE(ex1: String, ex2: String = "?") = ex1 + " ILIKE " + ex2
  def BETWEEN(ex: String, v1: String = "?", v2: String= "?") =
    ex + " BETWEEN " + v1 + " AND " + v2
  def IN(ex: String) = ex + " IN"
  def NOT_IN(ex: String) = ex + " NOT IN"
  def parameterizedIn(ex: String, params: Iterable[String]) =
    ex + " IN (" + params.mkString(", ") + ")"

  def AND = "AND"
  def OR = "OR"
  def NOT = "NOT"

  def ALL = "ALL"
  def SOME = "SOME"

  def EXISTS = "EXISTS"
  def NOT_EXISTS = "NOT EXISTS"

  /*!## Functions and others */

  def NULL = "NULL"
  def DISTINCT = "DISTINCT"
  def COUNT(ex: String) = "COUNT(" + ex + ")"
  def COUNT_DISTINCT(ex: String) = "COUNT(DISTINCT " + ex + ")"
  def MAX(ex: String) = "MAX(" + ex + ")"
  def MIN(ex: String) = "MIN(" + ex + ")"
  def SUM(ex: String) = "SUM(" + ex + ")"
  def AVG(ex: String) = "AVG(" + ex + ")"

  /*!## Set operations */

  def UNION = "UNION"
  def UNION_ALL = "UNION ALL"
  def EXCEPT = "EXCEPT"
  def EXCEPT_ALL = "EXCEPT ALL"
  def INTERSECT = "INTERSECT"
  def INTERSECT_ALL = "INTERSECT ALL"

  /*!## Order specificators */

  def asc = "ASC"
  def desc = "DESC"

  /*!## Param placeholders */

  def placeholder = "?"
  def xmlPlaceholder = "XMLPARSE(DOCUMENT ?)"

  /*!## Features Compliance */

  def supportsSchema: Boolean = true
  def supportsDropConstraints: Boolean = true

  /*!## Commons */

  def quoteLiteral(expr: String) = "'" + expr.replace("'", "''") + "'"

  def escapeParameter(value: Any): String = value match {
    case Some(v) => escapeParameter(v)
    case None | null => "NULL"
    case v => quoteLiteral(v.toString)
  }

  def relationQualifiedName(relation: Relation[_, _]) =
    if (supportsSchema) relation.schema.name + "." + relation.relationName
    else relation.relationName

  def alias(expression: String, alias: String) =
    expression + " AS " + alias

  def qualifyColumn(vh: ValueHolder[_, _], tableAlias: String) =
    tableAlias + "." + vh.name

  def on(expression: Expression) = "ON (" + expression.toInlineSql + ")"

  def not(expression: String) = "NOT (" + expression + ")"

  def subquery(expression: String, subquery: SQLQuery[_]) =
    expression + " ( " + subquery.toSql + " )"

  /*!## Data Definition Language */

  def constraintDefinition(constraint: Constraint) =
    "CONSTRAINT " + constraint.constraintName + " " + constraint.sqlDefinition

  def alterTable(rel: Relation[_, _], action: String) =
    "ALTER TABLE " + rel.qualifiedName + " " + action

  def alterTableAddConstraint(constraint: Constraint) =
    alterTable(constraint.relation, "ADD " + constraintDefinition(constraint));

  def alterTableDropConstraint(constraint: Constraint) =
    alterTable(constraint.relation, "DROP CONSTRAINT " + constraint.constraintName);

  def createSchema(schema: Schema) = "CREATE SCHEMA " + schema.name

  def dropSchema(schema: Schema) = "DROP SCHEMA " + schema.name + " CASCADE"

  def createTable[PK, R <: Record[PK, R]](table: Table[PK, R]) =
    "CREATE TABLE " + table.qualifiedName + " (" +
        table.fields.map(_.toSql).mkString(", ") +
        ", PRIMARY KEY (" + table.PRIMARY_KEY.name + "))"

  def dropTable[PK, R <: Record[PK, R]](table: Table[PK, R]) =
    "DROP TABLE " + table.qualifiedName

  def createView[PK, R <: Record[PK, R]](view: View[PK, R]) =
    "CREATE VIEW " + view.qualifiedName + " (" +
        view.fields.map(_.name).mkString(", ") + ") AS " +
        view.query.toInlineSql

  def dropView[PK, R <: Record[PK, R]](view: View[PK, R]) =
    "DROP VIEW " + view.qualifiedName

  def createIndex(idx: Index): String = {
    var result = "CREATE "
    if (idx.isUnique) result += "UNIQUE "
    result += "INDEX " + idx.name + " ON " + idx.relation.qualifiedName +
        " USING " + idx.usingClause + " (" + idx.expression + ")"
    if (idx.whereClause != EmptyPredicate)
      result += " WHERE " + idx.whereClause.toInlineSql
    result
  }

  def dropIndex(idx: Index) =
    "DROP INDEX " + idx.relation.schema.name + "." + idx.name

  def columnDefinition[R <: Record[_, R]](field: Field[_, R]): String = {
    var result = field.name + " " + field.sqlType
    if (field.isNotNull) result += " NOT NULL"
    if (field.isUnique) result += " UNIQUE"
    result += defaultExpression(field)
    result
  }

  def compositeFieldName(names: String*): String = names.mkString(", ")

  def initializeRelation[R <: Record[_, R]](relation: Relation[_, R]) {}

  def initializeField[R <: Record[_, R]](field: Field[_, R]) = field match {
    case f: AutoIncrementable[_, _]
      if (f.isAutoIncrement && !field.record.relation.isInstanceOf[View[_, R]]) => {
      val seqName = sequenceName(f)
      val seq = new SchemaObject {
        val objectName = "SEQUENCE " + seqName
        val sqlDrop = "DROP SEQUENCE " + seqName
        val sqlCreate = "CREATE SEQUENCE " + seqName
      }
      f.record.relation.addPreAux(seq)
    }
    case _ =>
  }

  def defaultExpression[R <: Record[_, R]](field: Field[_, R]): String =
    field match {
      case a: AutoIncrementable[_, _] if (a.isAutoIncrement) =>
        " DEFAULT NEXTVAL('" + sequenceName(field) + "')"
      case _ =>
        field.defaultExpression.map(" DEFAULT " + _).getOrElse("")
    }

  def sequenceName[R <: Record[_, R]](vh: ValueHolder[_, R]) =
    vh.record.relation.schema.name + "." +
        vh.record.relation.relationName + "_" + vh.name + "_seq"

  def uniqueKeyDefinition(uniq: UniqueKey) =
    "UNIQUE (" + uniq.columns.map(_.name).mkString(", ") + ")"

  def foreignKeyDefinition(fk: ForeignKey) =
    "FOREIGN KEY (" + fk.childColumns.map(_.name).mkString(", ") +
        ") REFERENCES " + fk.parentRelation.qualifiedName + " (" +
        fk.parentColumns.map(_.name).mkString(", ") + ") " +
        "ON DELETE " + fk.onDelete.toSql + " " +
        "ON UPDATE " + fk.onUpdate.toSql

  def checkConstraintDefinition(check: CheckConstraint) =
    "CHECK (" + check.expression + ")"

  /*!## Structured Query Language */

  def join(j: JoinNode[_, _, _, _]): String = joinInternal(j, null)

  protected def joinInternal(node: RelationNode[_, _], on: String): String = {
    var result = ""
    node match {
      case j: JoinNode[_, _, _, _] =>
        result += joinInternal(j.left, on) +
            " " + j.joinType.toSql + " " +
            joinInternal(j.right, j.sqlOn)
      case _ =>
        result += node.toSql
        if (on != null) result += " " + on
    }
    result
  }

  def select(q: Select[_]): String = {
    var result = "SELECT "
    if (q.isDistinct)
      result += "DISTINCT "
    result += q.projections.map(_.toSql).mkString(", ")
    if (q.fromClause.size > 0)
      result += " FROM " + q.fromClause.map(_.toSql).mkString(", ")
    if (q.whereClause != EmptyPredicate)
      result += " WHERE " + q.whereClause.toSql
    if (q.groupByClause != "")
      result += " GROUP BY " + q.groupByClause
    if (q.havingClause != EmptyPredicate)
      result += " HAVING " + q.havingClause.toSql
    q.setOps.foreach {
      case (op: SetOperation, subq: SQLQuery[_]) =>
        result += " " + op.toSql + " ( " + subq.toSql + " )"
      case _ =>
    }
    if (q.orderByClause.size > 0)
      result += " ORDER BY " + q.orderByClause.map(_.toSql).mkString(", ")
    if (q.limit > -1)
      result += " LIMIT " + q.limit
    if (q.offset > 0)
      result += " OFFSET " + q.offset
    result
  }

  def identityLastIdPredicate[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): Predicate =
    new SimpleExpression(node.alias + "." + node.relation.PRIMARY_KEY.name + " = LASTVAL()", Nil)

  def identityLastIdQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    new Select(expr[PK]("LASTVAL()"))

  def sequenceNextValQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    new Select(expr[PK]("NEXTVAL('" + sequenceName(node.relation.PRIMARY_KEY) + "')"))

  /*!## Data Manipulation Language */

  def insert[PK, R <: Record[PK, R]](dml: Insert[PK, R]): String = {
    var result = "INSERT INTO " + dml.relation.qualifiedName
    if (dml.fields.size > 0)
      result += " (" + dml.fields.map(_.name).mkString(", ") +
          ") VALUES (" + dml.fields.map(_.placeholder).mkString(", ") + ")"
    else result += " DEFAULT VALUES"
    result
  }

  def insertSelect[PK, R <: Record[PK, R]](dml: InsertSelect[PK, R]) =
    "INSERT INTO " + dml.relation.qualifiedName + " (" +
        dml.relation.fields.map(_.name).mkString(", ") + ") " + dml.query.toSql

  def update[PK, R <: Record[PK, R]](dml: Update[PK, R]): String = {
    var result = "UPDATE " + dml.node.toSql + " SET " +
        dml.setClause.map(f => f._1.name + " = " + f._1.placeholder).mkString(", ")
    if (dml.whereClause != EmptyPredicate) result += " WHERE " + dml.whereClause.toSql
    result
  }

  def delete[PK, R <: Record[PK, R]](dml: Delete[PK, R]): String = {
    var result = "DELETE FROM " + dml.node.toSql
    if (dml.whereClause != EmptyPredicate) result += " WHERE " + dml.whereClause.toSql
    result
  }

}
