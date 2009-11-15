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

class SimpleExpression(val criterion: Criterion[_],
                       val expression: String,
                       val parameters: Seq[Any])
    extends Predicate {

  def toSql = expression.replaceAll("\\{alias}", criterion.toSqlWhere)

}

/**
 * Aggregates multiple predicates with specified operator.
 */
class AggregatePredicate(val operator: String,
                         val predicates: Predicate *)
    extends Predicate {
  private var params: Seq[Any] = predicates.toList.flatMap(_.parameters)

  def parameters = params

  def toSql: String = {
    val pl = predicates.toList
    if (pl.size == 0) EmptyPredicate.toSql
    else pl.map(_.toSql).mkString(operator)
  }
}