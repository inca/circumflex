/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

package ru.circumflex.orm

import ORM._

/**
 * Predicates form a criteria tree that is used to filter results.
 */
trait Predicate extends SQLable {
  /**
   * The list of parameters that should be applied to this predicate.
   * Essentially these are set on JDBC prepared statement object.
   * The parameters count and order must match predicate's SQL representation.
   */
  def parameters: Seq[Any]
}

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

/**
 * Contains some common predicates.
 */
class SimpleExpressionHelper(val expr: String) {
  def eq(value: Any) = new SimpleExpression(expr + dialect.eq, List(value))

  def ne(value: Any) = new SimpleExpression(expr + dialect.ne, List(value))

  def gt(value: Any) = new SimpleExpression(expr + dialect.gt, List(value))

  def ge(value: Any) = new SimpleExpression(expr + dialect.ge, List(value))

  def lt(value: Any) = new SimpleExpression(expr + dialect.lt, List(value))

  def le(value: Any) = new SimpleExpression(expr + dialect.le, List(value))

  def isNull = new SimpleExpression(expr + dialect.isNull, Nil)

  def isNotNull = new SimpleExpression(expr + dialect.isNotNull, Nil)

  def like(value: String) = new SimpleExpression(expr + dialect.like, List(value))

  def ilike(value: String) = new SimpleExpression(expr + dialect.ilike, List(value))

  def in(params: Any*) =
    new SimpleExpression(expr + dialect.parameterizedIn(params), params.toList) 

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

  def toSql: String =
    if (predicates.size == 0) EmptyPredicate.toSql
    else "(" + predicates.map(_.toSql).mkString(operator) + ")"
}
