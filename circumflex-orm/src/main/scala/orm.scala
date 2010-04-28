package ru.circumflex.orm

import com.mchange.v2.c3p0.ComboPooledDataSource
import ru.circumflex.core.Circumflex
import java.sql.{Timestamp, PreparedStatement, ResultSet, Connection}
import java.util.Date
import javax.naming.InitialContext
import javax.sql.DataSource
import org.slf4j.LoggerFactory
import ORM._
import java.util.regex.Pattern

// ## Configuration

/**
 * `ORM` singleton aggregates all ORM-related interfaces into a single
 * configuration object.
 */
object ORM {

  protected[orm] val ormLog = LoggerFactory.getLogger("ru.circumflex.orm")

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


  // ### Global Configuration Objects

  /**
   * Connection provider.
   * Can be overriden with `orm.connectionProvider` configuration parameter.
   */
  val connectionProvider: ConnectionProvider = Circumflex.cfg("orm.connectionProvider") match {
    case Some(p: ConnectionProvider) => p
    case Some(c: Class[ConnectionProvider]) => c.newInstance
    case Some(s: String) => Circumflex.loadClass[ConnectionProvider](s)
        .newInstance
    case _ => DefaultConnectionProvider
  }

  /**
   * SQL dialect.
   * Can be overriden with `orm.dialect` configuration parameter.
   */
  val dialect: Dialect = Circumflex.cfg("orm.dialect") match {
    case Some(d: Dialect) => d
    case Some(c: Class[Dialect]) => c.newInstance
    case Some(s: String) => Circumflex.loadClass[Dialect](s)
        .newInstance
    case _ => DefaultDialect
  }

  /**
   * SQL type converter.
   * Can be overriden with `orm.typeConverter` configuration parameter.
   */
  val typeConverter: TypeConverter = Circumflex.cfg("orm.typeConverter") match {
    case Some(tc: TypeConverter) => tc
    case Some(c: Class[TypeConverter]) => c.newInstance
    case Some(s: String) => Circumflex.loadClass[TypeConverter](s)
        .newInstance
    case _ => DefaultTypeConverter
  }

  /**
   * The schema name which is used if not specified explicitly.
   * Can be overriden with `orm.defaultSchema` configuration parameter.
   */
  val defaultSchema = Circumflex.cfg("orm.defaultSchema") match {
    case Some(s: String) => new Schema(s)
    case _ => new Schema("public")
  }

  /**
   * Transaction manager.
   * Can be overriden with `orm.transactionManager` configuration parameter.
   */
  val transactionManager: TransactionManager = Circumflex.cfg("orm.transactionManager") match {
    case Some(tm: TransactionManager) => tm
    case Some(c: Class[TransactionManager]) => c.newInstance
    case Some(s: String) => Circumflex.loadClass[TransactionManager](s)
        .newInstance
    case _ => DefaultTransactionManager
  }

  /**
   * Shortcut for retrieving current transaction via `transactionManager.getTransaction`.
   */
  def tx = transactionManager.getTransaction

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

// ### Connection provider

/**
 * *Connection provider* is used to acquire JDBC connections throughout the application.
 */
trait ConnectionProvider {

  /**
   * Open new JDBC connection.
   */
  def openConnection: Connection

}

/**
 * Default `ConnectionProvider` implementation. It behaves as follows:
 *
 *  * if `orm.connection.datasource` is set, use it to acquire data source
 *  from JNDI;
 *  * if `orm.connection.datasource` is missing, construct a connection
 *  pool using [c3p0][] and following configuration parameters:
 *     * `orm.connection.driver`
 *     * `orm.connection.url`
 *     * `orm.connection.username`
 *     * `orm.connection.password`
 *   * set *auto-commit* for each connection to `false`
 *   * set the transaction isolation level to the value `orm.connection.isolation`
 *   (or use `READ COMMITTED` by default)
 *
 * If c3p0 data source is used you can fine tune it's configuration with `c3p0.properties`
 * file (see [c3p0 documentation][c3p0-cfg] for more details).
 *
 * Though `DefaultConnectionProvider` is an optimal choice for most applications, you
 * can create your own connection provider by implementing the `ConnectionProvider` trait
 * and setting the `orm.connectionProvider` configuration parameter.
 *
 *   [c3p0]: http://www.mchange.com/projects/c3p0
 *   [c3p0-cfg]: http://www.mchange.com/projects/c3p0/index.html#configuration_properties
 */
class DefaultConnectionProvider extends ConnectionProvider {

  protected val isolation: Int = Circumflex.cfg("orm.connection.isolation") match {
    case Some("none") => Connection.TRANSACTION_NONE
    case Some("read_uncommitted") => Connection.TRANSACTION_READ_UNCOMMITTED
    case Some("read_committed") => Connection.TRANSACTION_READ_COMMITTED
    case Some("repeatable_read") => Connection.TRANSACTION_REPEATABLE_READ
    case Some("serializable") => Connection.TRANSACTION_SERIALIZABLE
    case _ => {
      ormLog.info("Using READ COMMITTED isolation, override 'orm.connection.isolation' if necesssary.")
      Connection.TRANSACTION_READ_COMMITTED
    }
  }

  /**
   * Configure datasource instance. It is retrieved from JNDI if 'orm.connection.datasource'
   * is specified or is constructed using c3p0 otherwise.
   */
  protected val ds: DataSource = Circumflex.cfg("orm.connection.datasource") match {
    case Some(jndiName: String) => {
      val ctx = new InitialContext
      val ds = ctx.lookup(jndiName).asInstanceOf[DataSource]
      ormLog.info("Using JNDI datasource ({}).", jndiName)
      ds
    }
    case _ => {
      ormLog.info("Using c3p0 connection pooling.")
      val driver = Circumflex.cfg("orm.connection.driver") match {
        case Some(s: String) => s
        case _ =>
          throw new ORMException("Missing mandatory configuration parameter 'orm.connection.driver'.")
      }
      val url = Circumflex.cfg("orm.connection.url") match {
        case Some(s: String) => s
        case _ =>
          throw new ORMException("Missing mandatory configuration parameter 'orm.connection.url'.")
      }
      val username = Circumflex.cfg("orm.connection.username") match {
        case Some(s: String) => s
        case _ =>
          throw new ORMException("Missing mandatory configuration parameter 'orm.connection.username'.")
      }
      val password = Circumflex.cfg("orm.connection.password") match {
        case Some(s: String) => s
        case _ =>
          throw new ORMException("Missing mandatory configuration parameter 'orm.connection.password'.")
      }
      val ds = new ComboPooledDataSource()
      ds.setDriverClass(driver)
      ds.setJdbcUrl(url)
      ds.setUser(username)
      ds.setPassword(password)
      ds
    }
  }

  def dataSource: DataSource = ds

  /**
   * Open a new JDBC connection.
   */
  def openConnection: Connection = {
    val conn = dataSource.getConnection
    conn.setAutoCommit(false)
    conn.setTransactionIsolation(isolation)
    return conn
  }

}

object DefaultConnectionProvider extends DefaultConnectionProvider

// ### Type converter

/**
 * *Type converters* are used to read atomic values from JDBC result sets and to set
 * JDBC prepared statement values for execution. If you intend to use custom types,
 * provide your own implementation.
 */
trait TypeConverter {

  /**
   * Read a value from specified `ResultSet` at specified column `alias`.
   */
  def read(rs: ResultSet, alias: String): Any = {
    val result = rs.getObject(alias)
    if (rs.wasNull) return null
    else return result
  }

  /**
   * Write a value to specified `PreparedStatement` at specified `paramIndex`.
   */
  def write(st: PreparedStatement, parameter: Any, paramIndex: Int): Unit = parameter match {
    case Some(p) => write(st, p, paramIndex)
    case None | null => st.setObject(paramIndex, null)
    case value => st.setObject(paramIndex, convert(value))
  }

  /**
   * Convert a value.
   */
  def convert(value: Any): Any = value match {
    case (p: Date) => new Timestamp(p.getTime)
    case value => value
  }

  /**
   * Convert a value and return it literally (with quoting strings).
   */
  def toString(value: Any): String = convert(value) match {
    case None | null => dialect.NULL
    case s: String => dialect.quoteLiteral(s)
    case d: Timestamp => dialect.quoteLiteral(d.toString)
    case other => other.toString
  }
}

object DefaultTypeConverter extends TypeConverter
