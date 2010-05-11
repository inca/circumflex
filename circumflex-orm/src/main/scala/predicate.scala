package ru.circumflex.orm

import ORM._

// ## Predicate

/**
 * **Predicate** is essentially a `ParameterizedExpression`
 * that yields boolean value when executed by database.
 *
 * Predicates are designed to participate in `WHERE` clause of
 * SQL queries.
 */
trait Predicate extends ParameterizedExpression

/**
 * A predicate that always yields `true` when executed by
 * the database, it is used when the `WHERE` clause is empty.
 */
object EmptyPredicate extends Predicate {
  def toSql = dialect.emptyPredicate
  def parameters = Nil
}

/**
 * Simple expression.
 */
class SimpleExpression(val expression: String,
                       val parameters: Seq[Any])
    extends Predicate {
  def toSql = expression
}

/**
 * Aggregation of multiple `predicates` with specified `operator`.
 */
class AggregatePredicate(val operator: String,
                         protected var predicates: Seq[Predicate])
    extends Predicate {
  def parameters = predicates.flatMap(_.parameters)
  def add(predicate: Predicate*): this.type = {
    predicates ++= predicate.toList
    return this
  }
  def toSql: String =
    if (predicates.size == 0) EmptyPredicate.toSql
    else "(" + predicates.map(_.toSql).mkString(" " + operator + " ") + ")"
}

/**
 * An expression with subquery.
 */
class SubqueryExpression[T](expression: String,
                            val subquery: SQLQuery[T])
    extends SimpleExpression(
      dialect.subquery(expression, subquery),
      subquery.parameters)

// ## Helper

/**
 * A helper to construct some common predicates in a DSL-like way.
 */
class SimpleExpressionHelper(val expr: String) {

  /* ### Simple expressions */

  def EQ(value: Any) = new SimpleExpression(expr + " " + dialect.EQ, List(value))
  def _eq(value: Any) = EQ(value)

  def NE(value: Any) = new SimpleExpression(expr + " " + dialect.NE, List(value))
  def _ne(value: Any) = NE(value)

  def GT(value: Any) = new SimpleExpression(expr + " " + dialect.GT, List(value))
  def _gt(value: Any) = GT(value)

  def GE(value: Any) = new SimpleExpression(expr + " " + dialect.GE, List(value))
  def _ge(value: Any) = GE(value)

  def LT(value: Any) = new SimpleExpression(expr + " " + dialect.LT, List(value))
  def _lt(value: Any) = LT(value)

  def LE(value: Any) = new SimpleExpression(expr + " " + dialect.LE, List(value))
  def _le(value: Any) = LE(value)

  def isNull = new SimpleExpression(expr + " " + dialect.isNull, Nil)
  def IS_NULL = isNull

  def isNotNull = new SimpleExpression(expr + " " + dialect.isNotNull, Nil)
  def IS_NOT_NULL = isNotNull

  def like(value: String) = new SimpleExpression(expr + " " + dialect.like, List(value))
  def LIKE(value: String) = like(value)

  def ilike(value: String) = new SimpleExpression(expr + " " + dialect.ilike, List(value))
  def ILIKE(value: String) = ilike(value)

  def in(params: Any*) =
    new SimpleExpression(expr + " " + dialect.parameterizedIn(params), params.toList)
  def IN(params: Any*) = in(params: _*)

  def between(lowerValue: Any, upperValue: Any) =
    new SimpleExpression(expr + " " + dialect.between, List(lowerValue, upperValue))
  def BETWEEN(lowerValue: Any, upperValue: Any) = between(lowerValue, upperValue)

  /* ### Simple subqueries */

  def in(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.in, query)
  def IN(query: SQLQuery[_]) = in(query)
  def notIn(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.notIn, query)
  def NOT_IN(query: SQLQuery[_]) = notIn(query)

  def eqAll(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.EQ + " " + dialect.all, query)
  def EQ_ALL(query: SQLQuery[_]) = eqAll(query)
  def neAll(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.NE + " " + dialect.all, query)
  def NE_ALL(query: SQLQuery[_]) = neAll(query)
  def gtAll(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.GT + " " + dialect.all, query)
  def GT_ALL(query: SQLQuery[_]) = gtAll(query)
  def geAll(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.GE + " " + dialect.all, query)
  def GE_ALL(query: SQLQuery[_]) = geAll(query)
  def ltAll(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.LT + " " + dialect.all, query)
  def LT_ALL(query: SQLQuery[_]) = ltAll(query)
  def leAll(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.LE + " " + dialect.all, query)
  def LE_ALL(query: SQLQuery[_]) = leAll(query)

  def eqSome(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.EQ + " " + dialect.some, query)
  def EQ_SOME(query: SQLQuery[_]) = eqSome(query)
  def neSome(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.NE + " " + dialect.some, query)
  def NE_SOME(query: SQLQuery[_]) = neSome(query)
  def gtSome(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.GT + " " + dialect.some, query)
  def GT_SOME(query: SQLQuery[_]) = gtSome(query)
  def geSome(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.GE + " " + dialect.some, query)
  def GE_SOME(query: SQLQuery[_]) = geSome(query)
  def ltSome(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.LT + " " + dialect.some, query)
  def LT_SOME(query: SQLQuery[_]) = ltSome(query)
  def leSome(query: SQLQuery[_]) =
    new SubqueryExpression(expr + " " + dialect.LE + " " + dialect.some, query)
  def LE_SOME(query: SQLQuery[_]) = leSome(query)

}
