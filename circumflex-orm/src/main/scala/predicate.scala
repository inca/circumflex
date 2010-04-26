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
                         val predicates: Seq[Predicate])
    extends Predicate {
  private var params: Seq[Any] = predicates.flatMap(_.parameters)

  def parameters = params

  def toSql: String =
    if (predicates.size == 0) EmptyPredicate.toSql
    else "(" + predicates.map(_.toSql).mkString(" " + operator + " ") + ")"
}

/**
 * An expression with subquery.
 */
class SubqueryExpression(expression: String,
                         val subselect: Subselect)
    extends SimpleExpression(
      dialect.subquery(expression, subselect),
      subselect.parameters)

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

  def in(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.in, subselect)
  def IN(subselect: Subselect) = in(subselect)
  def notIn(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.notIn, subselect)
  def NOT_IN(subselect: Subselect) = notIn(subselect)

  def allEq(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.EQ + " " + dialect.all, subselect)
  def ALL_EQ(subselect: Subselect) = allEq(subselect)
  def allNe(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.NE + " " + dialect.all, subselect)
  def ALL_NE(subselect: Subselect) = allNe(subselect)
  def allGt(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.GT + " " + dialect.all, subselect)
  def ALL_GT(subselect: Subselect) = allGt(subselect)
  def allGe(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.GE + " " + dialect.all, subselect)
  def ALL_GE(subselect: Subselect) = allGe(subselect)
  def allLt(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.LT + " " + dialect.all, subselect)
  def ALL_LT(subselect: Subselect) = allLt(subselect)
  def allLe(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.LE + " " + dialect.all, subselect)
  def ALL_LE(subselect: Subselect) = allLe(subselect)

  def someEq(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.EQ + " " + dialect.some, subselect)
  def SOME_EQ(subselect: Subselect) = someEq(subselect)
  def someNe(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.NE + " " + dialect.some, subselect)
  def SOME_NE(subselect: Subselect) = someNe(subselect)
  def someGt(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.GT + " " + dialect.some, subselect)
  def SOME_GT(subselect: Subselect) = someGt(subselect)
  def someGe(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.GE + " " + dialect.some, subselect)
  def SOME_GE(subselect: Subselect) = someGe(subselect)
  def someLt(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.LT + " " + dialect.some, subselect)
  def SOME_LT(subselect: Subselect) = someLt(subselect)
  def someLe(subselect: Subselect) =
    new SubqueryExpression(expr + " " + dialect.LE + " " + dialect.some, subselect)
  def SOME_LE(subselect: Subselect) = someLe(subselect)

}
