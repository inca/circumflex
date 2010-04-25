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

  // ### Actions for Foreign Keys

  def fkNoAction = "NO ACTION"
  def fkCascade = "CASCADE"
  def fkRestrict = "RESTRICT"
  def fkSetNull = "SET NULL"
  def fkSetDefault = "SET DEFAULT"

  // ### Commons

  /**
   * Quote literal expression as described in SQL92 standard.
   */
  def quoteLiteral(expr: String) = "'" + expr.replace("'", "''") + "'"

  /**
   * Qualify relation name with it's schema.
   */
  def relationQualifiedName(relation: Relation[_]) = relation.schema + "." + relation.relationName

  /**
   * Just prepend specified expression with `DEFAULT` keyword.
   */
  def defaultExpression(expr: String) = "DEFAULT " + expr

  /**
   * Just append `AS` and specified `alias` to specified `expression`.
   */
  def scalarAlias(expression: String, alias: String) = expression + " AS " + alias

  /**
   * Qualify a column with table alias (e.g. "p.id")
   */
  def qualifyColumn(field: Field[_], tableAlias: String) =
    tableAlias + "." + field.name

  /**
   * Qualify column with table alias and append `AS` and it's alias.
   */
  def columnAlias(field: Field[_], columnAlias: String, tableAlias: String) =
    qualifyColumn(field, tableAlias) + " AS " + columnAlias



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
   * Produces unique constraint definition (e.g. "UNIQUE (name, value)").
   */
  def uniqueKeyDefinition(uniq: UniqueKey) =
    "UNIQUE (" + uniq.fields.map(_.name).mkString(",") + ")"


}
