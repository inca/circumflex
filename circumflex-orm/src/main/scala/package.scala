package ru.circumflex

import ru.circumflex.core._
import orm._
import java.util.regex.Pattern

/*!# The `orm` Package

Package `orm` contains different shortcuts, utilities, helpers and implicits --
the basis of DSL of Circumflex ORM.

You should import this package to use Circumflex ORM in your application:

    import ru.circumflex.orm._
*/
package object orm {

  val ORM_LOG = new Logger("ru.circumflex.orm")

  object jdbc {
    def autoClose[A <: {def close(): Unit}, B](obj: => A)
                                              (actions: A => B)
                                              (errors: Throwable => B): B =
      try {
        val res = obj
        try {
          return actions(res)
        } catch {
          case e => return errors(e)
        } finally {
          res.close
        }
      } catch {
        case e => return errors(e)
      }
  }

  // Configuration

  val connectionProvider: ConnectionProvider = cx.instantiate[ConnectionProvider](
    "orm.connectionProvider", new DefaultConnectionProvider)

  val typeConverter: TypeConverter = cx.instantiate[TypeConverter](
    "orm.typeConverter", new DefaultTypeConverter)

  val dialect: Dialect = cx.instantiate[Dialect]("orm.dialect", cx.get("orm.connection.url") match {
    case Some(url: String) =>
      if (url.startsWith("jdbc:postgresql:")) new PostgreSQLDialect
      else if (url.startsWith("jdbc:mysql:")) new MySQLDialect
      else if (url.startsWith("jdbc:oracle:")) new OracleDialect
      else if (url.startsWith("jdbc:h2:")) new H2Dialect
      else new Dialect
    case _ => new Dialect
  })

  val transactionManager: TransactionManager = cx.instantiate[TransactionManager](
    "orm.transactionManager", new DefaultTransactionManager)

  val defaultSchema: Schema = new Schema(
    cx.get("orm.defaultSchema").map(_.toString).getOrElse("public"))

  def cacheService = CacheService.get

  // Implicits

  implicit def association2field[K, C <: Record[_, C], P <: Record[K, P]](
      association: Association[K, C, P]): Field[K, C] = association.field
  implicit def relation2node[PK, R <: Record[PK, R]](relation: Relation[PK, R]): RelationNode[PK, R] =
    new RelationNode[PK, R](relation)
  implicit def node2relation[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): Relation[PK, R] =
    node.relation
  implicit def string2helper(expression: String): SimpleExpressionHelper =
    new SimpleExpressionHelper(expression)
  implicit def string2predicate(expression: String): Predicate =
    new SimpleExpression(expression, Nil)
  implicit def string2order(expression: String): Order =
    new Order(expression, Nil)
  implicit def paramExpr2predicate(expression: ParameterizedExpression): Predicate =
    new SimpleExpression(expression.toSql, expression.parameters)
  implicit def predicate2aggregateHelper(predicate: Predicate) =
    new AggregatePredicateHelper(predicate)
  implicit def predicate2string(predicate: Predicate): String = predicate.toInlineSql

  /*
  implicit def tuple2proj[T1, T2](
      t: (Projection[T1], Projection[T2])) =
    new Tuple2Projection(t._1, t._2)
  implicit def tuple3proj[T1, T2, T3](
      t: (Projection[T1], Projection[T2], Projection[T3])) =
    new Tuple3Projection(t._1, t._2, t._3)
  implicit def tuple4proj[T1, T2, T3, T4](
      t: (Projection[T1], Projection[T2], Projection[T3], Projection[T4])) =
    new Tuple4Projection(t._1, t._2, t._3, t._4)
  implicit def tuple5proj[T1, T2, T3, T4, T5](
      t: (Projection[T1], Projection[T2], Projection[T3], Projection[T4], Projection[T5])) =
    new Tuple5Projection(t._1, t._2, t._3, t._4, t._5)
  implicit def tuple6proj[T1, T2, T3, T4, T5, T6](
      t: (Projection[T1], Projection[T2], Projection[T3], Projection[T4], Projection[T5],
          Projection[T6])) =
    new Tuple6Projection(t._1, t._2, t._3, t._4, t._5, t._6)
  implicit def tuple7proj[T1, T2, T3, T4, T5, T6, T7](
      t: (Projection[T1], Projection[T2], Projection[T3], Projection[T4], Projection[T5],
          Projection[T6], Projection[T7])) =
    new Tuple7Projection(t._1, t._2, t._3, t._4, t._5, t._6, t._7)
  implicit def tuple8proj[T1, T2, T3, T4, T5, T6, T7, T8](
      t: (Projection[T1], Projection[T2], Projection[T3], Projection[T4], Projection[T5],
          Projection[T6], Projection[T7], Projection[T8])) =
    new Tuple8Projection(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)
  implicit def tuple9proj[T1, T2, T3, T4, T5, T6, T7, T8, T9](
      t: (Projection[T1], Projection[T2], Projection[T3], Projection[T4], Projection[T5],
          Projection[T6], Projection[T7], Projection[T8], Projection[T9])) =
    new Tuple9Projection(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
  implicit def tuple10proj[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
      t: (Projection[T1], Projection[T2], Projection[T3], Projection[T4], Projection[T5],
          Projection[T6], Projection[T7], Projection[T8], Projection[T9], Projection[T10])) =
    new Tuple10Projection(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)
  */

  // Constants

  val NO_ACTION = ForeignKeyAction(dialect.fkNoAction)
  val CASCADE = ForeignKeyAction(dialect.fkCascade)
  val RESTRICT = ForeignKeyAction(dialect.fkRestrict)
  val SET_NULL = ForeignKeyAction(dialect.fkSetNull)
  val SET_DEFAULT = ForeignKeyAction(dialect.fkSetDefault)

  val INNER = JoinType(dialect.innerJoin)
  val LEFT = JoinType(dialect.leftJoin)
  val RIGHT = JoinType(dialect.rightJoin)
  val FULL = JoinType(dialect.fullJoin)

  val OP_UNION = SetOperation(dialect.union)
  val OP_UNION_ALL = SetOperation(dialect.unionAll)
  val OP_EXCEPT = SetOperation(dialect.except)
  val OP_EXCEPT_ALL = SetOperation(dialect.exceptAll)
  val OP_INTERSECT = SetOperation(dialect.intersect)
  val OP_INTERSECT_ALL = SetOperation(dialect.intersectAll)

  // Predicates DSL

  def AND(predicates: Predicate*) =
    new AggregatePredicateHelper(predicates.head).AND(predicates.tail: _*)
  def OR(predicates: Predicate*) =
    new AggregatePredicateHelper(predicates.head).OR(predicates.tail: _*)
  def NOT(predicate: Predicate) =
    new SimpleExpression(dialect.not(predicate.toSql), predicate.parameters)
  /*
  def expr[T](expression: String): ExpressionProjection[T] =
    new ExpressionProjection[T](expression)
  def prepareExpr(expression: String, params: Pair[String, Any]*): SimpleExpression = {
    var sqlText = expression
    var parameters: Seq[Any] = Nil
    val paramsMap = Map[String, Any](params: _*)
    val pattern = Pattern.compile(":(\\w+)\\b")
    val matcher = pattern.matcher(expression)
    while(matcher.find) paramsMap.get(matcher.group(1)) match {
      case Some(param) => parameters ++= List(param)
      case _ => parameters ++= List(null)
    }
    sqlText = matcher.replaceAll("?")
    return new SimpleExpression(sqlText, parameters)
  }

  // Simple subqueries DSL

  def EXISTS(subquery: SQLQuery[_]) =
    new SubqueryExpression(dialect.exists, subquery)

  def NOT_EXISTS(subquery: SQLQuery[_]) =
    new SubqueryExpression(dialect.notExists, subquery)

  // Simple projections

  def COUNT(expr: String) =
    new ExpressionProjection[Long](dialect.count + "(" + expr + ")")
  def COUNT_DISTINCT(expr: String) =
    new ExpressionProjection[Long](
      dialect.count + "(" + dialect.distinct + " " + expr + ")")
  def MAX(expr: String) =
    new ExpressionProjection[Any](dialect.max + "(" + expr + ")")
  def MIN(expr: String) =
    new ExpressionProjection[Any](dialect.min + "(" + expr + ")")
  def SUM(expr: String) =
    new ExpressionProjection[Any](dialect.sum + "(" + expr + ")")
  def AVG(expr: String) =
    new ExpressionProjection[Any](dialect.avg + "(" + expr + ")")

  // Queries DSL

  def SELECT[T](projection: Projection[T]) = new Select(projection)
  def INSERT_INTO[R <: Record[R]](relation: Relation[R]) = new InsertSelectHelper(relation)
  def UPDATE[R <: Record[R]](node: RelationNode[R]) = new Update(node)
  def DELETE[R <: Record[R]](node: RelationNode[R]) = new Delete(node)

  */

}