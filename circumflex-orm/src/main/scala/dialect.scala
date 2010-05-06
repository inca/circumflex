package ru.circumflex.orm

// ## SQL dialect

/**
 * A default dialect singleton.
 *
 * If you feel that some of the statements do not work
 * with your RDBMS vendor, trace the exact method and provide it's
 * implementation in your own class. After that, set the `orm.dialect`
 * configuration parameter accordingly.
 */
object DefaultDialect extends Dialect

/**
 * This little thingy does all dirty SQL rendering. We are orienting the default
 * dialect on the world's most advanced open-source database, [PostgreSQL][psql].
 *
 *   [psql]: http://postgresql.org
 */
class Dialect {

  // ### SQL types

  def longType = "BIGINT"
  def integerType = "INTEGER"
  def numericType = "NUMERIC"
  def textType = "TEXT"
  def varcharType = "VARCHAR"
  def booleanType = "BOOLEAN"
  def dateType = "DATE"
  def timeType = "TIME"
  def timestampType = "TIMESTAMPTZ"

  // ### Actions for foreign keys

  def fkNoAction = "NO ACTION"
  def fkCascade = "CASCADE"
  def fkRestrict = "RESTRICT"
  def fkSetNull = "SET NULL"
  def fkSetDefault = "SET DEFAULT"

  // ### Join keywords

  def innerJoin = "INNER JOIN"
  def leftJoin = "LEFT JOIN"
  def rightJoin = "RIGHT JOIN"
  def fullJoin = "FULL JOIN"

  // ### Predicates

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

  // ### Functions and others

  def NULL = "NULL"
  def distinct = "DISTINCT"
  def count = "COUNT"
  def max = "MAX"
  def min = "MIN"
  def sum = "SUM"
  def avg = "AVG"

  // ### Set operations

  def union = "UNION"
  def unionAll = "UNION ALL"
  def except = "EXCEPT"
  def exceptAll = "EXCEPT ALL"
  def intersect = "INTERSECT"
  def intersectAll = "INTERSECT ALL"

  // ### Order specificators

  def asc = "ASC"
  def desc = "DESC"

  // ### Features compliance

  def supportsSchema_?(): Boolean = true
  def supportsDropConstraints_?(): Boolean = true

  // ### Commons

  /**
   * Quote literal expression as described in SQL92 standard.
   */
  def quoteLiteral(expr: String) = "'" + expr.replace("'", "''") + "'"

  /**
   * Qualify relation name with it's schema.
   */
  def relationQualifiedName(relation: Relation[_]) =
    relation.schema.name + "." + relation.relationName

  /**
   * Just prepend specified expression with `DEFAULT` keyword.
   */
  def defaultExpression(expr: String) = "DEFAULT " + expr

  /**
   * Just append `AS` and specified `alias` to specified `expression`.
   */
  def alias(expression: String, alias: String) = expression + " AS " + alias

  /**
   * Qualify a column with table alias (e.g. "p.id")
   */
  def qualifyColumn(field: Field[_], tableAlias: String) =
    tableAlias + "." + field.name

  /**
   * Take specified `expression` into parentheses and prepend `ON`.
   */
  def on(expression: String) = "ON (" + expression + ")"

  /**
   * Take specified `expression` in parentheses and prepend `NOT`.
   */
  def not(expression: String) = "NOT (" + expression + ")"

  /**
   * Take specified `subquery` into parentheses and prepend with
   * specified `expression`.
   */
  def subquery(expression: String, subquery: SQLQuery[_]) =
    expression + " ( " + subquery.sqlSelect + " )"

  // ### DDL

  /**
   * Produce a full definition of constraint (prepends the specific definition
   * with `CONSTRAINT` keyword and constraint name.
   */
  def constraintDefinition(constraint: Constraint) =
    "CONSTRAINT " + constraint.constraintName + " " + constraint.sqlDefinition

  /**
   * Produce `ALTER TABLE` statement with abstract action.
   */
  def alterTable(rel: Relation[_], action: String) =
    "ALTER TABLE " + rel.qualifiedName + " " + action

  /**
   * Produce `ALTER TABLE` statement with `ADD CONSTRAINT` action.
   */
  def alterTableAddConstraint(constraint: Constraint) =
    alterTable(constraint.relation, "ADD " + constraintDefinition(constraint));

  /**
   * Produce `ALTER TABLE` statement with `DROP CONSTRAINT` action.
   */
  def alterTableDropConstraint(constraint: Constraint) =
    alterTable(constraint.relation, "DROP CONSTRAINT " + constraint.constraintName);

  /**
   * Produces `CREATE SCHEMA` statement.
   */
  def createSchema(schema: Schema) = "CREATE SCHEMA " + schema.name

  /**
   * Produce `DROP SCHEMA` statement.
   */
  def dropSchema(schema: Schema) = "DROP SCHEMA " + schema.name

  /**
   * Produce `CREATE TABLE` statement without constraints.
   */
  def createTable(table: Table[_]) =
    "CREATE TABLE " + table.qualifiedName + " (" +
        table.fields.map(_.toSql).mkString(", ") +
        ", PRIMARY KEY (" + table.primaryKey.name + "))"

  /**
   * Produce `DROP TABLE` statement.
   */
  def dropTable(table: Table[_]) =
    "DROP TABLE " + table.qualifiedName

  /**
   * Produces `CREATE VIEW` statement.
   */
  def createView(view: View[_]) =
    "CREATE VIEW " + view.qualifiedName + " (" +
        view.fields.map(_.name).mkString(", ") + ") AS " +
        view.query.toInlineSql

  /**
   * Produce `DROP VIEW` statement.
   */
  def dropView(view: View[_]) =
    "DROP VIEW " + view.qualifiedName

  /**
   * Produce `CREATE INDEX` statement.
   */
  def createIndex(idx: Index): String = {
    var result = "CREATE "
    if (idx.unique_?) result += "UNIQUE "
    result += "INDEX " + idx.name + " ON " + idx.relation.qualifiedName +
        " USING " + idx.using + " (" + idx.expression + ")"
    if (idx.where != EmptyPredicate)
      result += " WHERE " + idx.where.toInlineSql
    return result
  }

  /**
   * Produce `DROP INDEX` statement.
   */
  def dropIndex(idx: Index) = "DROP INDEX " + idx.relation.schema.name + "." + idx.name

  /**
   * SQL definition for a column represented by specified `field`
   * (e.g. `mycolumn VARCHAR NOT NULL`).
   */
  def columnDefinition(field: Field[_]): String = {
    var result = field.name + " " + field.sqlType
    if (!field.nullable_?) result += " NOT NULL"
    field.default match {
      case Some(expr) => result += " " + expr
      case _ =>
    }
    return result
  }

  /**
   * Make necessary stuff for relation initialization.
   *
   * This implementation adds an auxiliary sequence for primary key
   * (sequences are supported by PostgreSQL, Oracle and DB2 dialects).
   */
  def initializeRelation(relation: Relation[_]): Unit = {
    val seqName = pkSequenceName(relation)
    val seq = new SchemaObject {
      val objectName = "SEQUENCE " + seqName
      val sqlDrop = "DROP SEQUENCE " + seqName
      val sqlCreate = "CREATE SEQUENCE " + seqName
    }
    relation.addPreAux(seq)
  }

  /**
   * An expression for primary key column.
   */
  def primaryKeyExpression(record: Record[_]) =
    "DEFAULT NEXTVAL('" + pkSequenceName(record.relation) + "')"

  protected def pkSequenceName(relation: Relation[_]) =
    relation.qualifiedName + "_" + relation.primaryKey.name + "_seq"

  /**
   * Produce unique constraint definition (e.g. `UNIQUE (name, value)`).
   */
  def uniqueKeyDefinition(uniq: UniqueKey) =
    "UNIQUE (" + uniq.fields.map(_.name).mkString(", ") + ")"

  /**
   * Produce foreign key constraint definition for association (e.g.
   * `FOREIGN KEY (country_id) REFERENCES country(id) ON DELETE CASCADE`).
   */
  def foreignKeyDefinition(fk: ForeignKey) =
    "FOREIGN KEY (" + fk.localFields.map(_.name).mkString(", ") +
        ") REFERENCES " + fk.foreignRelation.qualifiedName + " (" +
        fk.foreignFields.map(_.name).mkString(", ") + ") " +
        "ON DELETE " + fk.onDelete.toSql + " " +
        "ON UPDATE " + fk.onUpdate.toSql

  /**
   * Produces check constraint definition (e.g. `CHECK (index > 0)`).
   */
  def checkConstraintDefinition(check: CheckConstraint) =
    "CHECK (" + check.expression + ")"

  // ### SQL

  def lastIdExpression(node: RelationNode[_]) =
    node.alias + "." + node.relation.primaryKey.name + " = LASTVAL()"

  /**
   * Produce SQL representation of joined tree of relations (`JoinNode` instance).
   */
  def join(j: JoinNode[_, _]): String = joinInternal(j, null)

  /**
   * Some magic to convert join tree to SQL.
   */
  protected def joinInternal(node: RelationNode[_], on: String): String = {
    var result = ""
    node match {
      case j: JoinNode[_, _] =>
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
   * Produces `SELECT` statement (without `LIMIT`, `OFFSET` and `ORDER BY`
   * clauses).
   */
  def subselect(q: Subselect[_]): String = {
    var result = "SELECT " + q.projections.map(_.toSql).mkString(", ")
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
        result += " " + op.toSql + " ( " + subq.sqlSelect + " )"
      case _ =>
    }
    return result
  }

  /**
   * Produce `SELECT` statement with `?` parameters.
   */
  def select(q: Select[_]): String = {
    var result = subselect(q)
    if (q.orderBy.size > 0)
      result += " ORDER BY " + q.orderBy.map(_.toSql).mkString(", ")
    if (q.limit > -1)
      result += " LIMIT " + q.limit
    if (q.offset > 0)
      result += " OFFSET " + q.offset
    return result
  }

  // ### DML

  /**
   * Produce `INSERT INTO .. VALUES` statement for specified `record` and specified `fields`.
   */
  def insertRecord(record: Record[_], fields: Seq[Field[_]]) =
    "INSERT INTO " + record.relation.qualifiedName +
        " (" + fields.map(_.name).mkString(", ") +
        ") VALUES (" + fields.map(f => "?").mkString(", ") + ")"

  /**
   * Produce `UPDATE` statement with primary key criteria for specified `record` using specified
   * `fields` in the `SET` clause.
   */
  def updateRecord(record: Record[_], fields: Seq[Field[_]]): String =
    "UPDATE " + record.relation.qualifiedName +
        " SET " + fields.map(_.name + " = ?").mkString(", ") +
        " WHERE " + record.id.name + " = ?"

  /**
   * Produce `DELETE` statement for specified `record`.
   */
  def deleteRecord(record: Record[_]): String =
    "DELETE FROM " + record.relation.qualifiedName +
        " WHERE " + record.id.name + " = ?"

  /**
   * Produce `INSERT .. SELECT` statement.
   */
  def insertSelect(dml: InsertSelect[_]) = "INSERT INTO " + dml.relation.qualifiedName + " (" +
      dml.relation.fields.map(_.name).mkString(", ") + ") " + dml.query.toSql

  /**
   * Produce `UPDATE` statement.
   */
  def update(dml: Update[_]): String = {
    var result = "UPDATE " + dml.relation.qualifiedName + " SET " +
        dml.setClause.map(_._1.name + " = ?").mkString(", ")
    if (dml.where != EmptyPredicate) result += " WHERE " + dml.where.toSql
    return result
  }

  /**
   * Produce `DELETE` statement.
   */
  def delete(dml: Delete[_]): String = {
    var result = "DELETE FROM " + dml.relation.qualifiedName
    if (dml.where != EmptyPredicate) result += " WHERE " + dml.where.toSql
    return result
  }

}
