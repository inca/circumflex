package ru.circumflex.orm

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

  /*!## SQL types */

  def longType = "BIGINT"
  def integerType = "INTEGER"
  def numericType = "NUMERIC"
  def textType = "TEXT"
  def varcharType = "VARCHAR"
  def booleanType = "BOOLEAN"
  def dateType = "DATE"
  def timeType = "TIME"
  def timestampType = "TIMESTAMPTZ"
  def xmlType = "XML"

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

  def EQ = "= ?"
  def NE = "<> ?"
  def GT = "> ?"
  def GE = ">= ?"
  def LT = "< ?"
  def LE = "<= ?"

  def emptyPredicate = "1 = 1"
  def isNull = "IS NULL"
  def isNotNull = "IS NOT NULL"
  def like = "LIKE ?"
  def ilike = "ILIKE ?"
  def between = "BETWEEN ? AND ?"
  def in = "IN"
  def notIn = "NOT IN"
  def parameterizedIn(params: Seq[_]) =
    "IN (" + params.map(p => "?").mkString(", ") + ")"

  def and = "AND"
  def or = "OR"
  def not = "NOT"

  def all = "ALL"
  def some = "SOME"

  def exists = "EXISTS"
  def notExists = "NOT EXISTS"

  /*!## Functions and others */

  def NULL = "NULL"
  def distinct = "DISTINCT"
  def count = "COUNT"
  def max = "MAX"
  def min = "MIN"
  def sum = "SUM"
  def avg = "AVG"

  /*!## Set operations */

  def union = "UNION"
  def unionAll = "UNION ALL"
  def except = "EXCEPT"
  def exceptAll = "EXCEPT ALL"
  def intersect = "INTERSECT"
  def intersectAll = "INTERSECT ALL"

  /*!## Order specificators */

  def asc = "ASC"
  def desc = "DESC"

  /*!## Features Compliance */

  def supportsSchema_?(): Boolean = true
  def supportsDropConstraints_?(): Boolean = true

  /*!## Commons */

  /**
   * Quotes literal expression as described in SQL92 standard.
   */
  def quoteLiteral(expr: String) = "'" + expr.replace("'", "''") + "'"

  /**
   * Quotes identifier for dialects that support it.
   */
  def quoteIdentifer(identifier: String) = identifier

  /**
   * Qualifies relation name with it's schema.
   */
  def relationQualifiedName(relation: Relation[_, _]) =
    quoteIdentifer(relation.schema.name) + "." + quoteIdentifer(relation.relationName)

  /**
   * Just appends `AS` and specified `alias` to specified `expression`.
   */
  def alias(expression: String, alias: String) =
    expression + " AS " + quoteIdentifer(alias)

  /**
   * Qualifies a column with table alias (e.g. "p.id")
   */
  def qualifyColumn(field: Field[_, _], tableAlias: String) =
    tableAlias + "." + quoteIdentifer(field.name)

  /**
   * Takes specified `expression` into parentheses and prepend `ON`.
   */
  def on(expression: String) = "ON (" + expression + ")"

  /**
   * Takes specified `expression` in parentheses and prepend `NOT`.
   */
  def not(expression: String) = "NOT (" + expression + ")"

  /**
   * Takes specified `subquery` into parentheses and prepend with
   * specified `expression`.
   */
  def subquery(expression: String, subquery: SQLQuery[_]) =
    expression + " ( " + subquery.toSql + " )"

  /*!## Data Definition Language */

  /**
   * Produces a full definition of constraint (prepends the specific definition
   * with `CONSTRAINT` keyword and constraint name).
   */
  def constraintDefinition(constraint: Constraint) =
    "CONSTRAINT " + quoteIdentifer(constraint.constraintName) + " " + constraint.sqlDefinition

  /**
   * Produces an `ALTER TABLE` statement with specified abstract `action`.
   */
  def alterTable(rel: Relation[_, _], action: String) =
    "ALTER TABLE " + rel.qualifiedName + " " + action

  /**
   * Produces an `ALTER TABLE` statement with `ADD CONSTRAINT` action.
   */
  def alterTableAddConstraint(constraint: Constraint) =
    alterTable(constraint.relation, "ADD " + constraintDefinition(constraint));

  /**
   * Produces an `ALTER TABLE` statement with `DROP CONSTRAINT` action.
   */
  def alterTableDropConstraint(constraint: Constraint) =
    alterTable(constraint.relation, "DROP CONSTRAINT " + quoteIdentifer(constraint.constraintName));

  /**
   * Produces a `CREATE SCHEMA` statement.
   */
  def createSchema(schema: Schema) = "CREATE SCHEMA " + quoteIdentifer(schema.name)

  /**
   * Produces `DROP SCHEMA` statement.
   */
  def dropSchema(schema: Schema) = "DROP SCHEMA " + quoteIdentifer(schema.name) + " CASCADE"

  /**
   * Produces a `CREATE TABLE` statement without constraints.
   */
  def createTable[PK, R <: Record[PK, R]](table: Table[PK, R]) =
    "CREATE TABLE " + table.qualifiedName + " (" +
        table.fields.map(_.toSql).mkString(", ") +
        ", PRIMARY KEY (" + quoteIdentifer(table.PRIMARY_KEY.name) + "))"

  /**
   * Produces a `DROP TABLE` statement.
   */
  def dropTable[PK, R <: Record[PK, R]](table: Table[PK, R]) =
    "DROP TABLE " + table.qualifiedName

  /**
   * Produces a `CREATE VIEW` statement.
   */
  def createView[PK, R <: Record[PK, R]](view: View[PK, R]) =
    "CREATE VIEW " + view.qualifiedName + " (" +
        view.fields.map(f => quoteIdentifer(f.name)).mkString(", ") + ") AS " +
        view.query.toInlineSql

  /**
   * Produces a `DROP VIEW` statement.
   */
  def dropView[PK, R <: Record[PK, R]](view: View[PK, R]) =
    "DROP VIEW " + quoteIdentifer(view.qualifiedName)

  /**
   * Produces a `CREATE INDEX` statement.
   */
  def createIndex(idx: Index): String = {
    var result = "CREATE "
    if (idx.unique_?) result += "UNIQUE "
    result += "INDEX " + quoteIdentifer(idx.name) + " ON " + idx.relation.qualifiedName +
        " USING " + idx.using + " (" + idx.expression + ")"
    if (idx.where != EmptyPredicate)
      result += " WHERE " + idx.where.toInlineSql
    return result
  }

  /**
   * Produces a `DROP INDEX` statement.
   */
  def dropIndex(idx: Index) =
    "DROP INDEX " + quoteIdentifer(idx.relation.schema.name) + "." + quoteIdentifer(idx.name)

  /**
   * Produces an SQL definition for a column represented by specified `field`
   * (e.g. `mycolumn VARCHAR NOT NULL`).
   */
  def columnDefinition[R <: Record[_, R]](field: Field[_, R]): String = {
    var result = field.name + " " + field.sqlType
    if (field.notNull_?) result += " NOT NULL"
    result += defaultExpression(field)
    return result
  }

  /**
   * Performs dialect-specific relation initialization.
   */
  def initializeRelation[R <: Record[_, R]](relation: Relation[_, R]): Unit = {}

  /**
   * Performs dialect-specific field initialization.
   */
  def initializeField[R <: Record[_, R]](field: Field[_, R]): Unit = field match {
    case f: AutoIncrementable[_, _] if (f.autoIncrement_?) => {
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

  /**
   * Produces a `DEFAULT` expression for specified `field`.
   */
  def defaultExpression[R <: Record[_, R]](field: Field[_, R]): String =
    field match {
      case a: AutoIncrementable[_, _] if (a.autoIncrement_?) =>
        " DEFAULT NEXTVAL('" + sequenceName(field) + "')"
      case _ =>
        field.defaultExpression.map(" DEFAULT " + _).getOrElse("")
    }

  protected def sequenceName[R <: Record[_, R]](f: Field[_, R]) =
    quoteIdentifer(f.record.relation.schema.name) + "." +
        quoteIdentifer(f.record.relation.relationName + "_" + f.name + "_seq")

  /**
   * Produces a definition of unique constraint (e.g. `UNIQUE (name, value)`).
   */
  def uniqueKeyDefinition(uniq: UniqueKey) =
    "UNIQUE (" + uniq.fields.map(_.name).mkString(", ") + ")"

  /**
   * Produces a definition of foreign key constraint (e.g.
   * `FOREIGN KEY (country_id) REFERENCES country(id) ON DELETE CASCADE`).
   */
  def foreignKeyDefinition(fk: ForeignKey) =
    "FOREIGN KEY (" + fk.childFields.map(_.name).mkString(", ") +
        ") REFERENCES " + fk.parentRelation.qualifiedName + " (" +
        fk.parentFields.map(_.name).mkString(", ") + ") " +
        "ON DELETE " + fk.onDelete.toSql + " " +
        "ON UPDATE " + fk.onUpdate.toSql

  /**
   * Produces a definition of check constraint (e.g. `CHECK (index > 0)`).
   */
  def checkConstraintDefinition(check: CheckConstraint) =
    "CHECK (" + check.expression + ")"

  /*!## Structured Query Language */

  /**
   * Produces an SQL representation of join node.
   */
  def join(j: JoinNode[_, _, _, _]): String = joinInternal(j, null)

  /**
   * Some magic to convert join tree to SQL.
   */
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
    return result
  }

  /**
   * Produces a `SELECT` statement.
   */
  def select(q: Select[_]): String = {
    var result = "SELECT "
    if (q.distinct_?)
      result += "DISTINCT "
    result += q.projections.map(_.toSql).mkString(", ")
    if (q.from.size > 0)
      result += " FROM " + q.from.map(_.toSql).mkString(", ")
    if (q.where != EmptyPredicate)
      result += " WHERE " + q.where.toSql
    if (q.groupBy.size > 0)
      result += " GROUP BY " + q.groupBy.flatMap(_.sqlAliases).mkString(", ")
    if (q.having != EmptyPredicate)
      result += " HAVING " + q.having.toSql
    q.setOps.foreach {
      case (op: SetOperation, subq: SQLQuery[_]) =>
        result += " " + op.toSql + " ( " + subq.toSql + " )"
      case _ =>
    }
    if (q.orderBy.size > 0)
      result += " ORDER BY " + q.orderBy.map(_.toSql).mkString(", ")
    if (q.limit > -1)
      result += " LIMIT " + q.limit
    if (q.offset > 0)
      result += " OFFSET " + q.offset
    return result
  }

  /*!## Data Manipulation Language */

  /**
   * Produces an `INSERT INTO .. VALUES` statement for specified `record` and specified `fields`.
   */
  def insertRecord(record: Record[_, _], fields: Seq[Field[_, _]]) =
    "INSERT INTO " + record.relation.qualifiedName +
        " (" + fields.map(f => quoteIdentifer(f.name)).mkString(", ") +
        ") VALUES (" + fields.map(f => "?").mkString(", ") + ")"

  /**
   * Produces an `UPDATE` statement with primary key criteria for specified `record` using specified
   * `fields` in the `SET` clause.
   */
  def updateRecord(record: Record[_, _], fields: Seq[Field[_, _]]): String =
    "UPDATE " + record.relation.qualifiedName +
        " SET " + fields.map(f => quoteIdentifer(f.name) + " = ?").mkString(", ") +
        " WHERE " + quoteIdentifer(record.PRIMARY_KEY.name) + " = ?"

  /**
   * Produces a `DELETE` statement for specified `record`.
   */
  def deleteRecord(record: Record[_, _]): String =
    "DELETE FROM " + record.relation.qualifiedName +
        " WHERE " + quoteIdentifer(record.PRIMARY_KEY.name) + " = ?"

}
