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

object And extends Configurable {
  def apply(predicates: Predicate *) =
    new AggregatePredicate(configuration.dialect.and, predicates.toList)
}

object Or extends Configurable {
  def apply(predicates: Predicate *) =
    new AggregatePredicate(configuration.dialect.or, predicates.toList)
}