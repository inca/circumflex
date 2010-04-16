package ru.circumflex.orm

/* ## SQL dialect */

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

  /* ### SQL types */

  def longType = "bigint"
  def integerType = "integer"
  def numericType = "numeric"
  def stringType = "text"
  def varcharType = "varchar"
  def booleanType = "boolean"
  def dateType = "date"
  def timeType = "time"
  def timestampType = "timestamptz"

  /* ### Commons */

  def quoteLiteral(expr: String) = "'" + expr.replace("'", "''") + "'"

  /* ### DDL */

  /**
   * SQL definition for a column
   * (e.g. `mycolumn varchar not null`).
   */
  def columnDefinition(col: ColumnMeta[_]): String = {
    var result = col.columnName + " " + col.sqlType
    if (!col.nullable) result += " not null"
    col.default match {
      case Some(expr) => result += " " + expr
      case _ =>
    }
    return result
  }


}
