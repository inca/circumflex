package ru.circumflex

import _root_.ru.circumflex.orm._
import ORM._
import java.util.regex.Pattern

// ## ORM package object

package object orm {

  // ### Implicits

  implicit def relation2node[R <: Record[R]](relation: Relation[R]): RelationNode[R] =
    relation.as("this")
  implicit def string2helper(expression: String): SimpleExpressionHelper =
    new SimpleExpressionHelper(expression)
  implicit def string2predicate(expression: String): Predicate =
    new SimpleExpression(expression, Nil)
  implicit def paramExpr2predicate(expression: ParameterizedExpression): Predicate =
    new SimpleExpression(expression.toSql, expression.parameters)
  implicit def string2projection(expression: String): Projection[Any] =
    new ExpressionProjection[Any](expression)
  implicit def association2field(association: Association[_, _]): Field[Long] =
    association.field
  implicit def relation2recordSample[R <: Record[R]](relation: Relation[R]): R =
    relation.r
  // The most magical ones.
  implicit def node2record[R <: Record[R]](node: RelationNode[R]): R = {
    lastAlias(node.alias)
    return node.relation.recordSample
  }
  implicit def field2str(field: Field[_]): String = lastAlias match {
    case Some(alias) => alias + "." + field.name
    case None => field.name
  }
  implicit def field2helper(field: Field[_]) = new SimpleExpressionHelper(field2str(field))

  implicit def tuple2proj[T1, T2](
      t: Tuple2[Projection[T1],Projection[T2]]) =
    new Tuple2Projection(t._1, t._2)
  implicit def tuple3proj[T1, T2, T3](
      t: Tuple3[Projection[T1], Projection[T2], Projection[T3]]) =
    new Tuple3Projection(t._1, t._2, t._3)
  implicit def tuple4proj[T1, T2, T3, T4](
      t: Tuple4[Projection[T1], Projection[T2], Projection[T3], Projection[T4]]) =
    new Tuple4Projection(t._1, t._2, t._3, t._4)
  implicit def tuple5proj[T1, T2, T3, T4, T5](
      t: Tuple5[Projection[T1], Projection[T2], Projection[T3], Projection[T4], Projection[T5]]) =
    new Tuple5Projection(t._1, t._2, t._3, t._4, t._5)
  implicit def tuple6proj[T1, T2, T3, T4, T5, T6](
      t: Tuple6[Projection[T1], Projection[T2], Projection[T3], Projection[T4], Projection[T5],
          Projection[T6]]) =
    new Tuple6Projection(t._1, t._2, t._3, t._4, t._5, t._6)
  implicit def tuple7proj[T1, T2, T3, T4, T5, T6, T7](
      t: Tuple7[Projection[T1], Projection[T2], Projection[T3], Projection[T4], Projection[T5],
          Projection[T6], Projection[T7]]) =
    new Tuple7Projection(t._1, t._2, t._3, t._4, t._5, t._6, t._7)
  implicit def tuple8proj[T1, T2, T3, T4, T5, T6, T7, T8](
      t: Tuple8[Projection[T1], Projection[T2], Projection[T3], Projection[T4], Projection[T5],
          Projection[T6], Projection[T7], Projection[T8]]) =
    new Tuple8Projection(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)
  implicit def tuple9proj[T1, T2, T3, T4, T5, T6, T7, T8, T9](
      t: Tuple9[Projection[T1], Projection[T2], Projection[T3], Projection[T4], Projection[T5],
          Projection[T6], Projection[T7], Projection[T8], Projection[T9]]) =
    new Tuple9Projection(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
  implicit def tuple10proj[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
      t: Tuple10[Projection[T1], Projection[T2], Projection[T3], Projection[T4], Projection[T5],
          Projection[T6], Projection[T7], Projection[T8], Projection[T9], Projection[T10]]) =
    new Tuple10Projection(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)

  // ### Current Transaction Shortcuts

  /**
   * Shortcut for retrieving current transaction via `transactionManager.getTransaction`.
   */
  def tx = transactionManager.getTransaction
  def TX = tx
  def commit() = tx.commit()
  def COMMIT() = commit()
  def rollback() = tx.rollback()
  def ROLLBACK() = rollback()

  // ### Constants

  val NO_ACTION = ForeignKeyAction(dialect.fkNoAction)
  val CASCADE = ForeignKeyAction(dialect.fkCascade)
  val RESTRICT = ForeignKeyAction(dialect.fkRestrict)
  val SET_NULL = ForeignKeyAction(dialect.fkSetNull)
  val SET_DEFAULT = ForeignKeyAction(dialect.fkSetDefault)

  val INNER_JOIN = JoinType(dialect.innerJoin)
  val LEFT_JOIN = JoinType(dialect.leftJoin)
  val RIGHT_JOIN = JoinType(dialect.rightJoin)
  val FULL_JOIN = JoinType(dialect.fullJoin)

  val OP_UNION = SetOperation(dialect.union)
  val OP_UNION_ALL = SetOperation(dialect.unionAll)
  val OP_EXCEPT = SetOperation(dialect.except)
  val OP_EXCEPT_ALL = SetOperation(dialect.exceptAll)
  val OP_INTERSECT = SetOperation(dialect.intersect)
  val OP_INTERSECT_ALL = SetOperation(dialect.intersectAll)

  // ### SQL shortcuts

  // Predicates.

  def and(predicates: Predicate*) =
    new AggregatePredicate(dialect.and, predicates.toList)
  def AND(predicates: Predicate*) = and(predicates: _*)

  def or(predicates: Predicate*) =
    new AggregatePredicate(dialect.or, predicates.toList)
  def OR(predicates: Predicate*) = or(predicates: _*)

  def not(predicate: Predicate) =
    new SimpleExpression(dialect.not(predicate.toSql), predicate.parameters)
  def NOT(predicate: Predicate) = not(predicate)

  def expr[T](expression: String): ExpressionProjection[T] =
    new ExpressionProjection[T](expression)

  def prepareExpr(expression: String, params: Pair[String, Any]*): SimpleExpression = {
    var sqlText = expression
    var parameters: Seq[Any] = Nil
    val paramsMap = Map[String, Any](params: _*)
    val pattern = Pattern.compile(":([a-zA-Z_]+)\\b")
    val matcher = pattern.matcher(expression)
    while(matcher.find) paramsMap.get(matcher.group(1)) match {
      case Some(param) => parameters ++= List(param)
      case _ => parameters ++= List(null)
    }
    sqlText = matcher.replaceAll("?")
    return new SimpleExpression(sqlText, parameters)
  }

  // Simple subqueries.

  def exists(subquery: SQLQuery[_]) =
    new SubqueryExpression(dialect.exists, subquery)
  def EXISTS(subquery: SQLQuery[_]) = exists(subquery)

  def notExists(subquery: SQLQuery[_]) =
    new SubqueryExpression(dialect.notExists, subquery)
  def NOT_EXISTS(subquery: SQLQuery[_]) = notExists(subquery)

  // Simple projections.

  def count(expr: String) =
    new ExpressionProjection[Int](dialect.count + "(" + expr + ")")
  def COUNT(expr: String) = count(expr)

  def countDistinct(expr: String) =
    new ExpressionProjection[Int](
      dialect.count + "(" + dialect.distinct + " " + expr + ")")
  def COUNT_DISTINCT(expr: String) = countDistinct(expr)

  def max(expr: String) =
    new ExpressionProjection[Any](dialect.max + "(" + expr + ")")
  def MAX(expr: String) = max(expr)

  def min(expr: String) =
    new ExpressionProjection[Any](dialect.min + "(" + expr + ")")
  def MIN(expr: String) = min(expr)

  def sum(expr: String) =
    new ExpressionProjection[Any](dialect.sum + "(" + expr + ")")
  def SUM(expr: String) = sum(expr)

  def avg(expr: String) =
    new ExpressionProjection[Any](dialect.avg + "(" + expr + ")")
  def AVG(expr: String) = avg(expr)

  // Query DSLs

  def select[T](projection: Projection[T]) = new Select(projection)
  def SELECT[T](projection: Projection[T]) = select(projection)

}
