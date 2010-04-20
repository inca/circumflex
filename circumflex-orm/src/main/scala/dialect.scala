package ru.circumflex.orm

// ## SQL dialect

/**
 * A default dialect singleton.
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

  // ### Commons

  /**
   * Quote literal expression as described in SQL92 standard.
   */
  def quoteLiteral(expr: String) = "'" + expr.replace("'", "''") + "'"

  /**
   * Qualify relation name with it's schema.
   */
  def relationQualifiedName(relation: Relation[_]) = relation.schema + "." + relation.relationName

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
   * SQL definition for a column
   * (e.g. `mycolumn VARCHAR NOT NULL`).
   */
  def columnDefinition(col: Column): String = {
    var result = col.columnName + " " + col.sqlType
    if (!col.nullable_?) result += " NOT NULL"
    col.default match {
      case Some(expr) => result += " " + expr
      case _ =>
    }
    return result
  }

  /**
   * Produces unique constraint definition (e.g. "UNIQUE (name, value)").
   */
  def uniqueKeyDefinition(uniq: UniqueKey) =
    "unique (" + uniq.columns.map(_.columnName).mkString(",") + ")"


}
