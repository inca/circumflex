package ru.circumflex.orm

/**
 * Predicates form a criteria tree that is used to filter results.
 */
trait Predicate {
  /**
   * SQL representation of this predicate for use in WHERE clause.
   * It should resolve to parameterized expression that yields boolean SQL value
   * when executed.
   */
  def toSql: String
}

abstract class SimpleExpression(val projection: Projection[_])
    extends Predicate {

  def expression: String

}