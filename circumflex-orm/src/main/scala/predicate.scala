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

  def in(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.in, subselect)
  def IN(subselect: Subselect[_]) = in(subselect)
  def notIn(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.notIn, subselect)
  def NOT_IN(subselect: Subselect[_]) = notIn(subselect)

  def eqAll(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.EQ + " " + dialect.all, subselect)
  def EQ_ALL(subselect: Subselect[_]) = eqAll(subselect)
  def neAll(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.NE + " " + dialect.all, subselect)
  def NE_ALL(subselect: Subselect[_]) = neAll(subselect)
  def gtAll(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.GT + " " + dialect.all, subselect)
  def GT_ALL(subselect: Subselect[_]) = gtAll(subselect)
  def geAll(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.GE + " " + dialect.all, subselect)
  def GE_ALL(subselect: Subselect[_]) = geAll(subselect)
  def ltAll(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.LT + " " + dialect.all, subselect)
  def LT_ALL(subselect: Subselect[_]) = ltAll(subselect)
  def leAll(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.LE + " " + dialect.all, subselect)
  def LE_ALL(subselect: Subselect[_]) = leAll(subselect)

  def eqSome(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.EQ + " " + dialect.some, subselect)
  def EQ_SOME(subselect: Subselect[_]) = eqSome(subselect)
  def neSome(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.NE + " " + dialect.some, subselect)
  def NE_SOME(subselect: Subselect[_]) = neSome(subselect)
  def gtSome(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.GT + " " + dialect.some, subselect)
  def GT_SOME(subselect: Subselect[_]) = gtSome(subselect)
  def geSome(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.GE + " " + dialect.some, subselect)
  def GE_SOME(subselect: Subselect[_]) = geSome(subselect)
  def ltSome(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.LT + " " + dialect.some, subselect)
  def LT_SOME(subselect: Subselect[_]) = ltSome(subselect)
  def leSome(subselect: Subselect[_]) =
    new SubqueryExpression(expr + " " + dialect.LE + " " + dialect.some, subselect)
  def LE_SOME(subselect: Subselect[_]) = leSome(subselect)

}
