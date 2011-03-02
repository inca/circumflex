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

  def driverClass: String =
    throw new ORMException("Missing mandatory configuration parameter 'orm.connection.driver'.")

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

  def supportsSchema_?(): Boolean = true
  def supportsDropConstraints_?(): Boolean = true

  /*!## Commons */

  /**
   * Quotes literal expression as described in SQL92 standard.
   */
  def quoteLiteral(expr: String) = "'" + expr.replace("'", "''") + "'"

  /**
   * Escapes JDBC-compliant parameter
   */
  def escapeParameter(value: Any): String = value match {
    case Some(v) => escapeParameter(v)
    case None | null => "NULL"
    case v => quoteLiteral(v.toString)
  }

  /**
   * Qualifies relation name with it's schema.
   */
  def relationQualifiedName(relation: Relation[_, _]) =
    if (supportsSchema_?) relation.schema.name + "." + relation.relationName
    else relation.relationName


  /**
   * Just appends `AS` and specified `alias` to specified `expression`.
   */
  def alias(expression: String, alias: String) =
    expression + " AS " + alias

  /**
   * Qualifies a column with table alias (e.g. "p.id")
   */
  def qualifyColumn(vh: ValueHolder[_, _], tableAlias: String) =
    tableAlias + "." + vh.name

  /**
   * Takes specified `expression` into parentheses and prepend `ON`.
   */
  def ON(expression: Expression) = "ON (" + expression.toInlineSql + ")"

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
    "CONSTRAINT " + constraint.constraintName + " " + constraint.sqlDefinition

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
    alterTable(constraint.relation, "DROP CONSTRAINT " + constraint.constraintName);

  /**
   * Produces a `CREATE SCHEMA` statement.
   */
  def createSchema(schema: Schema) = "CREATE SCHEMA " + schema.name

  /**
   * Produces `DROP SCHEMA` statement.
   */
  def dropSchema(schema: Schema) = "DROP SCHEMA " + schema.name + " CASCADE"

  /**
   * Produces a `CREATE TABLE` statement without constraints.
   */
  def createTable[PK, R <: Record[PK, R]](table: Table[PK, R]) =
    "CREATE TABLE " + table.qualifiedName + " (" +
        table.fields.map(_.toSql).mkString(", ") +
        ", PRIMARY KEY (" + table.PRIMARY_KEY.name + "))"

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
        view.fields.map(_.name).mkString(", ") + ") AS " +
        view.query.toInlineSql

  /**
   * Produces a `DROP VIEW` statement.
   */
  def dropView[PK, R <: Record[PK, R]](view: View[PK, R]) =
    "DROP VIEW " + view.qualifiedName

  /**
   * Produces a `CREATE INDEX` statement.
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
   * Produces a `DROP INDEX` statement.
   */
  def dropIndex(idx: Index) =
    "DROP INDEX " + idx.relation.schema.name + "." + idx.name

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
   * Produces a name from multiple fields (generally by delimiting them with comma).
   */
  def compositeFieldName(names: String*): String = names.mkString(", ")

  /**
   * Performs dialect-specific relation initialization.
   */
  def initializeRelation[R <: Record[_, R]](relation: Relation[_, R]): Unit = {}

  /**
   * Performs dialect-specific field initialization.
   */
  def initializeField[R <: Record[_, R]](field: Field[_, R]): Unit = field match {
    case f: AutoIncrementable[_, _]
      if (f.autoIncrement_? && !field.record.relation.isInstanceOf[View[_, R]]) => {
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

  /**
   * Produces a name for database sequence.
   */
  def sequenceName[R <: Record[_, R]](vh: ValueHolder[_, R]) =
    vh.record.relation.schema.name + "." +
        vh.record.relation.relationName + "_" + vh.name + "_seq"

  /**
   * Produces a definition of unique constraint (e.g. `UNIQUE (name, value)`).
   */
  def uniqueKeyDefinition(uniq: UniqueKey) =
    "UNIQUE (" + uniq.columns.map(_.name).mkString(", ") + ")"

  /**
   * Produces a definition of foreign key constraint (e.g.
   * `FOREIGN KEY (country_id) REFERENCES country(id) ON DELETE CASCADE`).
   */
  def foreignKeyDefinition(fk: ForeignKey) =
    "FOREIGN KEY (" + fk.childColumns.map(_.name).mkString(", ") +
        ") REFERENCES " + fk.parentRelation.qualifiedName + " (" +
        fk.parentColumns.map(_.name).mkString(", ") + ") " +
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

  /**
   * Returns a predicate expression for querying the last inserted record
   * for `IdentityGenerator`.
   */
  def identityLastIdPredicate[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): Predicate =
    new SimpleExpression(node.alias + "." + node.relation.PRIMARY_KEY.name + " = LASTVAL()", Nil)

  /**
   * Returns a query which retrieves the last generated identity value for `IdentityGenerator`.
   */
  def identityLastIdQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    new Select(expr[PK]("LASTVAL()"))

  /**
   * Returns a query which retrieves the next sequence value for the primary key of specified `node`. 
   */
  def sequenceNextValQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    new Select(expr[PK]("NEXTVAL('" + sequenceName(node.relation.PRIMARY_KEY) + "')"))

  /*!## Data Manipulation Language */

  /**
   * Produces an `INSERT INTO .. VALUES` statement for specified `record` and specified `fields`.
   */
  def insert[PK, R <: Record[PK, R]](dml: Insert[PK, R]): String =
    "INSERT INTO " + dml.relation.qualifiedName +
        " (" + dml.fields.map(_.name).mkString(", ") +
        ") VALUES (" + dml.fields.map(_.placeholder).mkString(", ") + ")"

  /**
   * Produces an `INSERT .. SELECT` statement.
   */
  def insertSelect[PK, R <: Record[PK, R]](dml: InsertSelect[PK, R]) =
    "INSERT INTO " + dml.relation.qualifiedName + " (" +
        dml.relation.fields.map(_.name).mkString(", ") + ") " + dml.query.toSql

  /**
   * Produces an `UPDATE` statement.
   */
  def update[PK, R <: Record[PK, R]](dml: Update[PK, R]): String = {
    var result = "UPDATE " + dml.node.toSql + " SET " +
        dml.setClause.map(f => f._1.name + " = " + f._1.placeholder).mkString(", ")
    if (dml.where != EmptyPredicate) result += " WHERE " + dml.where.toSql
    return result
  }

  /**
   * Produces a `DELETE` statement.
   */
  def delete[PK, R <: Record[PK, R]](dml: Delete[PK, R]): String = {
    var result = "DELETE FROM " + dml.node.toSql
    if (dml.where != EmptyPredicate) result += " WHERE " + dml.where.toSql
    return result
  }

}
