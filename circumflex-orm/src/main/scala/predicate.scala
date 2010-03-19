package ru.circumflex.orm

import ORM._

/**
 * Predicates form a criteria tree that is used to filter results.
 */
trait Predicate extends SQLFragment

/**
 * Used to represent an empty WHERE clause (equivalent to always-true condition).
 */
object EmptyPredicate extends Predicate {
  def toSql = dialect.dummy

  def parameters = Nil
}

class SimpleExpression(val expression: String,
                       val parameters: Seq[Any])
    extends Predicate {

  def toSql = expression

}

class SubqueryExpression(expression: String,
                         val subselect: Subselect)
    extends SimpleExpression(expression, subselect.parameters) {

  override def toSql = dialect.subquery(expression, subselect)

}

/**
 * Contains some common predicates.
 */
class SimpleExpressionHelper(val expr: String) {

  /* Simple expressions */

  def eq(value: Any) = new SimpleExpression(expr + " = ?", List(value))
  def ne(value: Any) = new SimpleExpression(expr + " <> ?", List(value))
  def gt(value: Any) = new SimpleExpression(expr + " > ?", List(value))
  def ge(value: Any) = new SimpleExpression(expr + " >= ?", List(value))
  def lt(value: Any) = new SimpleExpression(expr + " < ?", List(value))
  def le(value: Any) = new SimpleExpression(expr + " <= ?", List(value))
  def isNull = new SimpleExpression(expr + " is null", Nil)
  def isNotNull = new SimpleExpression(expr + " is not null", Nil)
  def like(value: String) = new SimpleExpression(expr + " like ?", List(value))
  def ilike(value: String) = new SimpleExpression(expr + " ilike ?", List(value))
  def in(params: Any*) =
    new SimpleExpression(expr + dialect.parameterizedIn(params), params.toList) 
  def between(lowerValue: Any, upperValue: Any) =
    new SimpleExpression(expr + " between ? and ?", List(lowerValue, upperValue))

  /* Simple subqueries */

  def in(subselect: Subselect) =
    new SubqueryExpression(expr + " in", subselect)
  def notIn(subselect: Subselect) =
    new SubqueryExpression(expr + " not in", subselect)

  def allEq(subselect: Subselect) =
    new SubqueryExpression(expr + " = all", subselect)
  def allNe(subselect: Subselect) =
    new SubqueryExpression(expr + " <> all", subselect)
  def allGt(subselect: Subselect) =
    new SubqueryExpression(expr + " > all", subselect)
  def allGe(subselect: Subselect) =
    new SubqueryExpression(expr + " >= all", subselect)
  def allLt(subselect: Subselect) =
    new SubqueryExpression(expr + " < all", subselect)
  def allLe(subselect: Subselect) =
    new SubqueryExpression(expr + " <= all", subselect)

  def someEq(subselect: Subselect) =
    new SubqueryExpression(expr + " = some", subselect)
  def someNe(subselect: Subselect) =
    new SubqueryExpression(expr + " <> some", subselect)
  def someGt(subselect: Subselect) =
    new SubqueryExpression(expr + " > some", subselect)
  def someGe(subselect: Subselect) =
    new SubqueryExpression(expr + " >= some", subselect)
  def someLt(subselect: Subselect) =
    new SubqueryExpression(expr + " < some", subselect)
  def someLe(subselect: Subselect) =
    new SubqueryExpression(expr + " <= some", subselect)

}

/**
 * Aggregates multiple predicates with specified operator.
 */
class AggregatePredicate(val operator: String,
                         val predicates: Seq[Predicate])
    extends Predicate {
  private var params: Seq[Any] = predicates.flatMap(_.parameters)

  def parameters = params

  def toSql: String =
    if (predicates.size == 0) EmptyPredicate.toSql
    else "(" + predicates.map(_.toSql).mkString(operator) + ")"

}
