package ru.circumflex.orm

/**
 * Predicates form a criteria tree that is used to filter results.
 */
trait Predicate extends Configurable {
  /**
   * SQL representation of this predicate for use in WHERE clause.
   * It should resolve to parameterized expression that yields boolean SQL value
   * when executed.
   */
  def toSql: String
  /**
   * The list of parameters that should be applied to this predicate.
   * Essentially these are set on JDBC prepared statement object.
   * The parameters count and order must match predicate's SQL representation.
   */
  def parameters: Seq[Any]

  override def toString = toSql
}

/**
 * Used to represent an empty WHERE clause (equivalent to always-true condition).
 */
object EmptyPredicate extends Predicate {
  def toSql = configuration.dialect.dummy
  def parameters = Nil
}

class SimpleExpression(val expression: String,
                       val parameters: Seq[Any])
    extends Predicate {
  def toSql = expression
}

/**
 * Contains some common predicates.
 */
class SimpleExpressionHelper(val expr: String) extends Configurable {
  def eq(value: Any) = new SimpleExpression(expr + dialect.eq, List(value))
  def ne(value: Any) = new SimpleExpression(expr + dialect.ne, List(value))
  def gt(value: Any) = new SimpleExpression(expr + dialect.gt, List(value))
  def ge(value: Any) = new SimpleExpression(expr + dialect.ge, List(value))
  def lt(value: Any) = new SimpleExpression(expr + dialect.lt, List(value))
  def le(value: Any) = new SimpleExpression(expr + dialect.le, List(value))
  def isNull = new SimpleExpression(expr + dialect.isNull, Nil)
  def isNotNull = new SimpleExpression(expr + dialect.isNotNull, Nil)
  def like(value: String) = new SimpleExpression(expr + dialect.like, List(value))
  def between(lowerValue: Any, upperValue: Any) =
    new SimpleExpression(expr + dialect.between, List(lowerValue, upperValue))
}

/**
 * Aggregates multiple predicates with specified operator.
 */
class AggregatePredicate(val operator: String,
                         val predicates: Seq[Predicate])
    extends Predicate {
  private var params: Seq[Any] = predicates.flatMap(_.parameters)

  def parameters = params

  def toSql: String = if (predicates.size == 0)
    EmptyPredicate.toSql
  else predicates.map(_.toSql).mkString(operator)
}

/**
 * Some common helpers for making up predicates.
 */
object Predicates extends Configurable {

  implicit def stringTonHelper(str: String): SimpleExpressionHelper =
    new SimpleExpressionHelper(str)

  implicit def fieldProjectionToHelper(f: FieldProjection[_]): SimpleExpressionHelper =
    new SimpleExpressionHelper(f.expr)

  def and(predicates: Predicate *) =
    new AggregatePredicate(configuration.dialect.and, predicates.toList)

  def or(predicates: Predicate *) =
    new AggregatePredicate(configuration.dialect.or, predicates.toList)
}