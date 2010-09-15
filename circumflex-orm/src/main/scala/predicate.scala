package ru.circumflex.orm

/*!# Predicates

`Predicate` is essentially a parameterized expression which yields boolean
value when executed by database.

Predicates are designed to participate in `WHERE` clauses of SQL queries.
*/

trait Predicate extends ParameterizedExpression

/**
 * Always yields `true` when executed by database, used to designate an empty
 * `WHERE` clause.
 */
object EmptyPredicate extends Predicate {
  def parameters: scala.Seq[Any] = Nil
  def toSql: String = dialect.emptyPredicate
}

/**
 * A predicate consisting of specified `expression` and specified `parameters`.
 */
class SimpleExpression(val expression: String,
                       val parameters: Seq[Any])
    extends Predicate {
  def toSql = expression
}

/**
 * Aggregates specified `predicates` with specified `operator` (for example, `OR`).
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
 * A predicate which consists of specified `expression` and specified `subquery`.
 */
/*
class SubqueryExpression[T](expression: String,
                            val subquery: SQLQuery[T])
    extends SimpleExpression(
      dialect.subquery(expression, subquery),
      subquery.parameters)
      */

/*! `SimpleExpressionHelper` is used to compose predicates in a DSL-style.
`String` expressions are converted to `SimpleExpressionHelper` implicitly.
 */
class SimpleExpressionHelper(val expr: String) {

  // Simple expressions

  def EQ(value: Any) = new SimpleExpression(expr + " " + dialect.EQ, List(value))
  def NE(value: Any) = new SimpleExpression(expr + " " + dialect.NE, List(value))
  def GT(value: Any) = new SimpleExpression(expr + " " + dialect.GT, List(value))
  def GE(value: Any) = new SimpleExpression(expr + " " + dialect.GE, List(value))
  def LT(value: Any) = new SimpleExpression(expr + " " + dialect.LT, List(value))
  def LE(value: Any) = new SimpleExpression(expr + " " + dialect.LE, List(value))
  def IS_NULL = new SimpleExpression(expr + " " + dialect.isNull, Nil)
  def IS_NOT_NULL = new SimpleExpression(expr + " " + dialect.isNotNull, Nil)
  def LIKE(value: Any) = new SimpleExpression(expr + " " + dialect.like, List(value))
  def ILIKE(value: Any) = new SimpleExpression(expr + " " + dialect.ilike, List(value))
  def IN(params: Any*) =
    new SimpleExpression(expr + " " + dialect.parameterizedIn(params), params.toList)
  def BETWEEN(lowerValue: Any, upperValue: Any) =
    new SimpleExpression(expr + " " + dialect.between, List(lowerValue, upperValue))

  // Simple subqueries
  /*
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
  */
}

/*! `AggregatePredicateHelper` is used to compose predicates using infix notation. */
class AggregatePredicateHelper(predicate: Predicate) {
  def AND(predicates: Predicate*) =
    new AggregatePredicate(dialect.and, predicate::predicates.toList)

  def OR(predicates: Predicate*) =
    new AggregatePredicate(dialect.or, predicate::predicates.toList)
}