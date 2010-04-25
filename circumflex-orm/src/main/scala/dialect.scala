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
  def stringType = "TEXT"
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
  def parameterizedIn(params: Seq[_]) =
    "IN (" + params.map(p => "?").mkString(", ") + ")"

  def and = "AND"
  def or = "OR"
  def not = "NOT"

  // ### Set operations

  def union = "UNION"
  def unionAll = "UNION ALL"
  def except = "EXCEPT"
  def exceptAll = "EXCEPT ALL"
  def intersect = "INTERSECT"
  def intersectAll = "INTERSECT ALL"

  // ### Features compliance

  def supportsSchema_?(): Boolean = true
  def supportDropConstraints_?(): Boolean = true

  // ### Commons

  /**
   * Quote literal expression as described in SQL92 standard.
   */
  def quoteLiteral(expr: String) = "'" + expr.replace("'", "''") + "'"

  /**
   * Qualify relation name with it's schema.
   */
  def relationQualifiedName(relation: Relation[_]) =
    relation.schema + "." + relation.relationName

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
   * SQL definition for a column represented by specified `field`
   * (e.g. `mycolumn VARCHAR NOT NULL`).
   */
  def columnDefinition(field: Field[_]): String = {
    var result = field.name + " " + field.sqlType
    if (field.isInstanceOf[NotNullField[_]]) result += " NOT NULL"
    field.default match {
      case Some(expr) => result += " " + expr
      case _ =>
    }
    return result
  }

  /**
   * Produces unique constraint definition (e.g. `UNIQUE (name, value)`).
   */
  def uniqueKeyDefinition(uniq: UniqueKey) =
    "UNIQUE (" + uniq.fields.map(_.name).mkString(",") + ")"

  /**
   * Produces foreign key constraint definition for association (e.g.
   * `FOREIGN KEY (country_id) REFERENCES country(id) ON DELETE CASCADE`).
   */
  def foreignKeyDefinition(fk: ForeignKey) =
    "FOREIGN KEY (" + fk.localFields.map(_.name).mkString(", ") +
            ") REFERENCES " + fk.foreignRelation.qualifiedName + " (" +
            fk.foreignFields.map(_.name).mkString(", ") + ") " +
            "ON DELETE " + fk.onDelete.toSql + " " +
            "ON UPDATE " + fk.onUpdate.toSql

  // ### SQL

  /**
   * Produces SQL representation of joined tree of relations (`JoinNode` instance).
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
  def subselect(q: Subselect): String = {
    var result = "SELECT " + q.projections.map(_.toSql).mkString(", ")
    if (q.relations.size > 0)
      result += " FROM " + q.relations.map(_.toSql).mkString(", ")
    if (q.where != EmptyPredicate)
      result += " WHERE " + q.where.toSql
    if (q.groupBy.size > 0)
      result += " GROUP BY " + q.groupBy.flatMap(_.sqlAliases).mkString(", ")
    if (q.having != EmptyPredicate)
      result += " HAVING " + q.having.toSql
    q.setOps.foreach {
      case (op: SetOperation, subq: Subselect) =>
        result += " " + op.toSql + " ( " + subq.toSubselectSql + " )"
      case _ =>
    }
    return result
  }

}
