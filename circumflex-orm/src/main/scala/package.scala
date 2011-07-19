package ru.circumflex

import core._
import java.util.regex.Pattern
import net.sf.ehcache.CacheManager
import collection.mutable.Stack

/*!# The `orm` Package

Package `orm` contains different shortcuts, utilities, helpers and implicits --
the basis of DSL of Circumflex ORM.

You should import this package to use Circumflex ORM in your application:

    import ru.circumflex.orm._
*/
package object orm {

  val ORM_LOG = new Logger("ru.circumflex.orm")

  // Commons

  val dialect: Dialect = cx.instantiate[Dialect]("orm.dialect", cx.get("orm.connection.url") match {
    case Some(url: String) =>
      if (url.startsWith("jdbc:postgresql:")) new PostgreSQLDialect
      else if (url.startsWith("jdbc:mysql:")) new MySQLDialect
      else if (url.startsWith("jdbc:oracle:")) new OracleDialect
      else if (url.startsWith("jdbc:h2:")) new H2Dialect
      else if (url.startsWith("jdbc:sqlserver:")) new MSSQLDialect
      else if (url.startsWith("jdbc:db2:")) new DB2Dialect
      else new Dialect
    case _ => new Dialect
  })

  val connectionProvider: ConnectionProvider = cx.instantiate[ConnectionProvider](
    "orm.connectionProvider", new DefaultConnectionProvider)

  val typeConverter: TypeConverter = cx.instantiate[TypeConverter](
    "orm.typeConverter", new TypeConverter)

  val transactionManager: TransactionManager = cx.instantiate[TransactionManager](
    "orm.transactionManager", new DefaultTransactionManager)

  val defaultSchema: Schema = new Schema(
    cx.get("orm.defaultSchema").map(_.toString).getOrElse("public"))

  val ehcacheManager: CacheManager = cx.instantiate[CacheManager](
    "orm.ehcache.manager", new CacheManager())

  def contextCache = CacheService.get

  def tx: Transaction = transactionManager.get
  def COMMIT() {
    tx.commit()
  }
  def ROLLBACK() {
    tx.rollback()
  }

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

  implicit def vh2colExpr[T, R <: Record[_, R]](vh: ValueHolder[T, R]): ColumnExpression[T, R] =
    new ColumnExpression(vh)
  implicit def str2expr(str: String): SimpleExpression = prepareExpr(str)

  // for predicates

  implicit def string2helper(expression: String): SimpleExpressionHelper =
    new SimpleExpressionHelper(expression)
  implicit def string2predicate(expression: String): Predicate =
    new SimpleExpression(expression, Nil)
  implicit def expr2predicate(expression: Expression): Predicate =
    new SimpleExpression(expression.toSql, expression.parameters)
  implicit def predicate2aggregateHelper(predicate: Predicate) =
    new AggregatePredicateHelper(predicate)
  implicit def boolean2predicate(f: BooleanField[_]): Predicate =
    string2predicate(f.aliasedName)

  // for orders

  implicit def string2order(expression: String): Order =
    new Order(expression, Nil)
  implicit def vh2order(vh: ValueHolder[_, _]): Order =
    new Order(vh.aliasedName, Nil)

  // for projections

  implicit def string2projection(expression: String): Projection[Any] =
    new ExpressionProjection[Any](expression)
  implicit def vh2projection[T](vh: ValueHolder[T, _]): Projection[T] =
    new ExpressionProjection[T](vh.aliasedName)

  implicit def pair2proj[T1, T2](t: (Projection[T1], Projection[T2])) =
    new PairProjection(t._1, t._2)

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

  val OP_UNION = SetOperation(dialect.UNION)
  val OP_UNION_ALL = SetOperation(dialect.UNION_ALL)
  val OP_EXCEPT = SetOperation(dialect.EXCEPT)
  val OP_EXCEPT_ALL = SetOperation(dialect.EXCEPT_ALL)
  val OP_INTERSECT = SetOperation(dialect.INTERSECT)
  val OP_INTERSECT_ALL = SetOperation(dialect.INTERSECT_ALL)

  // Predicates DSL

  def AND(predicates: Predicate*) =
    new AggregatePredicateHelper(predicates.head).AND(predicates.tail: _*)
  def OR(predicates: Predicate*) =
    new AggregatePredicateHelper(predicates.head).OR(predicates.tail: _*)
  def NOT(predicate: Predicate) =
    new SimpleExpression(dialect.not(predicate.toSql), predicate.parameters)
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
    new SimpleExpression(sqlText, parameters)
  }

  // Simple subqueries DSL

  def EXISTS(subquery: SQLQuery[_]) =
    new SubqueryExpression(dialect.EXISTS, subquery)

  def NOT_EXISTS(subquery: SQLQuery[_]) =
    new SubqueryExpression(dialect.NOT_EXISTS, subquery)

  // Simple projections

  def COUNT(expr: Expression): Projection[Long] =
    new ExpressionProjection[Long](dialect.COUNT(expr.toSql))
  def COUNT_DISTINCT(expr: Expression): Projection[Long] =
    new ExpressionProjection[Long](dialect.COUNT_DISTINCT(expr.toSql))
  def MAX(expr: Expression): Projection[Any] =
    new ExpressionProjection[Any](dialect.MAX(expr.toSql))
  def MIN(expr: Expression) =
    new ExpressionProjection[Any](dialect.MIN(expr.toSql))
  def SUM(expr: Expression) =
    new ExpressionProjection[Any](dialect.SUM(expr.toSql))
  def AVG(expr: Expression) =
    new ExpressionProjection[Any](dialect.AVG(expr.toSql))

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
