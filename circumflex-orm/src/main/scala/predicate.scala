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
 * `EmptyPredicate` are not included into expressions.
 */
class AggregatePredicate(val operator: String,
                         protected var _predicates: Seq[Predicate])
    extends Predicate {
  def parameters = predicates.flatMap(_.parameters)
  def add(predicate: Predicate*): this.type = {
    _predicates ++= predicate.toList
    return this
  }
  def predicates: Seq[Predicate] = _predicates.flatMap {
    case EmptyPredicate => None
    case p: AggregatePredicate if (p.predicates.size == 0) => None
    case p: AggregatePredicate if (p.predicates.size == 1) =>
      Some(p.predicates(0))
    case p => Some(p)
  }
  def toSql: String = {
    val p = predicates
    if (p.size == 0) EmptyPredicate.toSql
    else "(" + p.map(_.toSql).mkString(" " + operator + " ") + ")"
  }
}

/**
 * A predicate which consists of specified `expression` and specified `subquery`.
 */
class SubqueryExpression[T](expression: String,
                            val subquery: SQLQuery[T])
    extends SimpleExpression(
      dialect.subquery(expression, subquery), subquery.parameters)

/*! `SimpleExpressionHelper` is used to compose predicates in a DSL-style.
`String` expressions are converted to `SimpleExpressionHelper` implicitly.
 */
class SimpleExpressionHelper(val expr: String) {

  // Simple expressions

  def EQ(value: Any) = new SimpleExpression(dialect.EQ(expr), List(value))
  def NE(value: Any) = new SimpleExpression(dialect.NE(expr), List(value))
  def GT(value: Any) = new SimpleExpression(dialect.GT(expr), List(value))
  def GE(value: Any) = new SimpleExpression(dialect.GE(expr), List(value))
  def LT(value: Any) = new SimpleExpression(dialect.LT(expr), List(value))
  def LE(value: Any) = new SimpleExpression(dialect.LE(expr), List(value))
  def IS_NULL = new SimpleExpression(dialect.IS_NULL(expr), Nil)
  def IS_NOT_NULL = new SimpleExpression(dialect.IS_NOT_NULL(expr), Nil)
  def LIKE(value: Any) = new SimpleExpression(dialect.LIKE(expr), List(value))
  def ILIKE(value: Any) = new SimpleExpression(dialect.ILIKE(expr), List(value))
  def IN(params: Any*) = new SimpleExpression(
    dialect.parameterizedIn(expr, params.map(p => "?")), params.toList)
  def BETWEEN(lowerValue: Any, upperValue: Any) = new SimpleExpression(
    dialect.BETWEEN(expr), List(lowerValue, upperValue))

  // Simple subqueries

  def IN(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.IN(expr), query)
  def NOT_IN(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.NOT_IN(expr), query)

  def EQ_ALL(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.EQ(expr, dialect.ALL), query)
  def NE_ALL(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.NE(expr, dialect.ALL), query)
  def GT_ALL(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.GT(expr, dialect.ALL), query)
  def GE_ALL(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.GE(expr, dialect.ALL), query)
  def LT_ALL(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.LT(expr, dialect.ALL), query)
  def LE_ALL(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.LE(expr, dialect.ALL), query)

  def EQ_SOME(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.EQ(expr, dialect.SOME), query)
  def NE_SOME(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.NE(expr, dialect.SOME), query)
  def GT_SOME(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.GT(expr, dialect.SOME), query)
  def GE_SOME(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.GE(expr, dialect.SOME), query)
  def LT_SOME(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.LT(expr, dialect.SOME), query)
  def LE_SOME(query: SQLQuery[_]) =
    new SubqueryExpression(dialect.LE(expr, dialect.SOME), query)
}

/*! `AggregatePredicateHelper` is used to compose predicates using infix notation. */
class AggregatePredicateHelper(predicate: Predicate) {
  def AND(predicates: Predicate*) =
    new AggregatePredicate(dialect.AND, predicate::predicates.toList)
  def OR(predicates: Predicate*) =
    new AggregatePredicate(dialect.OR, predicate::predicates.toList)
}