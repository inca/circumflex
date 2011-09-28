package ru.circumflex

import core._
import collection.mutable.Stack
import java.util.regex.Pattern

/*!# The `orm` Package

Package `orm` contains different shortcuts, utilities, helpers and implicits --
the basis of DSL of Circumflex ORM.

You should import this package to use Circumflex ORM in your application:

    import ru.circumflex.orm._
*/
package object orm {

  val ORM_LOG = new Logger("ru.circumflex.orm")

  lazy val DEFAULT_ORM_CONF = cx.instantiate[ORMConfiguration](
    "orm.configuration", new SimpleORMConfiguration(""))
  def ormConf = ctx.get("orm.conf") match {
    case Some(c: ORMConfiguration) => c
    case _ => DEFAULT_ORM_CONF
  }

  def tx: Transaction = ormConf.transactionManager.get
  def COMMIT() {
    tx.commit()
  }
  def ROLLBACK() {
    tx.rollback()
  }

  def using[A](newConf: ORMConfiguration)(block: => A): A =
    Context.executeInNew { ctx =>
      ctx.update("orm.conf", newConf)
      block
    }

  def usingAll[A](newConfs: ORMConfiguration*)(block: ORMConfiguration => A): Seq[A] =
    newConfs.map { c => using(c) { block(c) } }

  /*! ## Alias Stack

  Circumflex ORM offers nice DSL to reference fields of aliased tables:

      val co = Country AS "co"
      val predicate = co.name EQ "Switzerland"

  In this example `RelationNode[Country]` with alias `"co"` is implicitly converted
  into `Country`, its underlying `Relation`, because only that relation owns field
  `name`. However, the information about the alias is lost during this conversion.
  We use `aliasStack` to remember it during conversion so it can be accessed later.
  */
  object aliasStack {
    protected def _stack: Stack[String] = ctx.get("orm.aliasStack") match {
      case Some(s: Stack[String]) => s
      case _ =>
        val s = Stack[String]()
        ctx += "orm.aliasStack" -> s
        s
    }
    def pop(): Option[String] = if (_stack.size == 0) None else Some(_stack.pop())
    def push(alias: String) {
      _stack.push(alias)
    }
  }

  // Pimping core libs

  implicit def str2expr(str: String): Expression = prepareExpr(str)
  implicit def string2exprHelper(expression: String): SimpleExpressionHelper =
    new SimpleExpressionHelper(expression)
  implicit def string2nativeHelper(expression: String): NativeQueryHelper =
    new NativeQueryHelper(expression)
  implicit def pair2proj[T1, T2](t: (Projection[T1], Projection[T2])) =
    new PairProjection(t._1, t._2)

  // Constants

  val NO_ACTION = ForeignKeyAction(ormConf.dialect.fkNoAction)
  val CASCADE = ForeignKeyAction(ormConf.dialect.fkCascade)
  val RESTRICT = ForeignKeyAction(ormConf.dialect.fkRestrict)
  val SET_NULL = ForeignKeyAction(ormConf.dialect.fkSetNull)
  val SET_DEFAULT = ForeignKeyAction(ormConf.dialect.fkSetDefault)

  val INNER = JoinType(ormConf.dialect.innerJoin)
  val LEFT = JoinType(ormConf.dialect.leftJoin)
  val RIGHT = JoinType(ormConf.dialect.rightJoin)
  val FULL = JoinType(ormConf.dialect.fullJoin)

  val OP_UNION = SetOperation(ormConf.dialect.UNION)
  val OP_UNION_ALL = SetOperation(ormConf.dialect.UNION_ALL)
  val OP_EXCEPT = SetOperation(ormConf.dialect.EXCEPT)
  val OP_EXCEPT_ALL = SetOperation(ormConf.dialect.EXCEPT_ALL)
  val OP_INTERSECT = SetOperation(ormConf.dialect.INTERSECT)
  val OP_INTERSECT_ALL = SetOperation(ormConf.dialect.INTERSECT_ALL)

  // Predicates DSL

  def AND(predicates: Predicate*) =
    new AggregatePredicateHelper(predicates.head).AND(predicates.tail: _*)
  def OR(predicates: Predicate*) =
    new AggregatePredicateHelper(predicates.head).OR(predicates.tail: _*)
  def NOT(predicate: Predicate) =
    new SimpleExpression(ormConf.dialect.not(predicate.toSql), predicate.parameters)

  def prepareExpr(expression: String, params: Pair[String, Any]*): SimpleExpression = {
    var sqlText = expression
    var parameters: Seq[Any] = Nil
    val paramsMap = Map[String, Any](params: _*)
    val pattern = Pattern.compile(":(\\w+)\\b")
    val matcher = pattern.matcher(expression)
    while (matcher.find) {
      val name = matcher.group(1)
      paramsMap.get(name) match {
        case Some(param) => parameters ++= List(param)
        case _ => parameters ++= List(":" + name)
      }
    }
    sqlText = matcher.replaceAll("?")
    new SimpleExpression(sqlText, parameters)
  }

  // Simple subqueries DSL

  def EXISTS(subquery: SQLQuery[_]) =
    new SubqueryExpression(ormConf.dialect.EXISTS, subquery)
  def NOT_EXISTS(subquery: SQLQuery[_]) =
    new SubqueryExpression(ormConf.dialect.NOT_EXISTS, subquery)

  // Simple projections

  def expr[T](expression: String): ExpressionProjection[T] =
    new ExpressionProjection[T](expression)
  def COUNT(expr: Expression): Projection[Long] =
    new ExpressionProjection[Long](ormConf.dialect.COUNT(expr.toSql))
  def COUNT_DISTINCT(expr: Expression): Projection[Long] =
    new ExpressionProjection[Long](ormConf.dialect.COUNT_DISTINCT(expr.toSql))
  def MAX[T](expr: Expression) =
    new ExpressionProjection[T](ormConf.dialect.MAX(expr.toSql))
  def MIN[T](expr: Expression) =
    new ExpressionProjection[T](ormConf.dialect.MIN(expr.toSql))
  def SUM[T](expr: Expression) =
    new ExpressionProjection[T](ormConf.dialect.SUM(expr.toSql))
  def AVG[T](expr: Expression) =
    new ExpressionProjection[T](ormConf.dialect.AVG(expr.toSql))

  // Queries DSL

  def SELECT[T](p1: Projection[T], p2: Projection[_], pn: Projection[_]*) = {
    val projections = List(p1, p2) ++ pn
    new Select(new AliasMapProjection(projections))
  }
  def SELECT[T](projection: Projection[T]): Select[T] = new Select(projection)
  def INSERT_INTO[PK, R <: Record[PK, R]](relation: Relation[PK, R]) =
    new InsertSelectHelper(relation)
  def UPDATE[PK, R <: Record[PK, R]](node: RelationNode[PK, R]) =
    new Update(node)
  def DELETE[PK, R <: Record[PK, R]](node: RelationNode[PK, R]) =
    new Delete(node)

}
