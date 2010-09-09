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

  // SQL types

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

  // Actions for foreign keys

  def fkNoAction = "NO ACTION"
  def fkCascade = "CASCADE"
  def fkRestrict = "RESTRICT"
  def fkSetNull = "SET NULL"
  def fkSetDefault = "SET DEFAULT"

  // Join keywords

  def innerJoin = "INNER JOIN"
  def leftJoin = "LEFT JOIN"
  def rightJoin = "RIGHT JOIN"
  def fullJoin = "FULL JOIN"

  // Predicates

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

  // Functions and others

  def NULL = "NULL"
  def distinct = "DISTINCT"
  def count = "COUNT"
  def max = "MAX"
  def min = "MIN"
  def sum = "SUM"
  def avg = "AVG"

  // Set operations

  def union = "UNION"
  def unionAll = "UNION ALL"
  def except = "EXCEPT"
  def exceptAll = "EXCEPT ALL"
  def intersect = "INTERSECT"
  def intersectAll = "INTERSECT ALL"

  // Order specificators

  def asc = "ASC"
  def desc = "DESC"

  // Features compliance

  def supportsSchema_?(): Boolean = true
  def supportsDropConstraints_?(): Boolean = true

  // Commons

  /**
   * Quote literal expression as described in SQL92 standard.
   */
  def quoteLiteral(expr: String) = "'" + expr.replace("'", "''") + "'"

  /**
   * Quote identifier for dialects that support it.
   */
  def quoteIdentifer(identifier: String) = identifier

  // Columns definition

  /**
   * The `DEFAULT` expression inside column's definition.
   */
  def defaultExpression(expr: String): String = "DEFAULT " + expr

}
