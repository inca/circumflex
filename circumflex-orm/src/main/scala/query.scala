package ru.circumflex.orm

import java.sql.{ResultSet, PreparedStatement}
import jdbc._

/*!# Querying

SQL and DML queries form the heart of Circumflex ORM DSL.

Common features implemented in the `Query` trait are *named parameters*
which allow query reuse and *ensuring alias uniqueness* which prevents
implicit relation node aliases from colliding within a single query.

The `SQLQuery` trait represents data-retrieval queries which usually employ
the `executeQuery` method of JDBC `PreparedStatement` and process JDBC
`ResultSet`.

The `DMLQuery` trait represents data-manipulation queries which usually
employ the `executeUpdate` method of JDBC `PreparedStatement` and return
the number of affected rows.
*/
trait Query extends SQLable with ParameterizedExpression with Cloneable {

  protected var aliasCounter = 0;

  /**
   * Generates an alias to eliminate duplicates within query.
   */
  protected def nextAlias: String = {
    aliasCounter += 1
    return "this_" + aliasCounter
  }

  /**
   * Sets the parameters of specified `PreparedStatement` of this query starting
   * from specified `index`. Because `Query` objects can be nested, this method returns
   * the new starting index of prepared statement parameter.
   */
  def setParams(st: PreparedStatement, index: Int): Int = {
    var paramsCounter = index;
    parameters.foreach(p => {
      typeConverter.write(st, convertNamedParam(p), paramsCounter)
      paramsCounter += 1
    })
    return paramsCounter
  }

  protected var _namedParams: Map[String, Any] = Map()

  def renderParams: Seq[Any] = parameters.map(p => convertNamedParam(p))

  def set(name: String, value: Any): this.type = {
    _namedParams += name -> value
    return this
  }

  protected def convertNamedParam(param: Any): Any = param match {
    case s: Symbol => lookupNamedParam(s.name)
    case s: String if (s.startsWith(":")) => lookupNamedParam(s)
    case _ => param
  }

  protected def lookupNamedParam(name: String): Any =
    _namedParams.get(name.replaceAll("^:", "")) match {
      case Some(p) => p
      case _ => name
    }

  override def clone(): this.type = super.clone.asInstanceOf[this.type]

  override def toString = toSql
}

/*! The `SQLQuery` trait defines a contract for data-retrieval queries.
It's only type parameter `T` designates the query result type (it is
determined by specified `projections`).
 */
abstract class SQLQuery[T](val projection: Projection[T]) extends Query {

  /**
   * Forms the `SELECT` clause of query. In normal circumstances this list
   * should only consist of single `projection` element; but if `GROUP_BY`
   * clause specifies projections that are not yet a part of the `SELECT`
   * clause, then they are added here implicitly but are not processed.
   */
  def projections: Seq[Projection[_]] = List(projection)

  /**
   * Makes sure that `projections` with alias `this` are assigned query-unique alias.
   */
  protected def ensureProjectionAlias[T](projection: Projection[T]): Unit =
    projection match {
      case p: AtomicProjection[_] if (p.alias == "this") => p.AS(nextAlias)
      case p: CompositeProjection[_] =>
        p.subProjections.foreach(ensureProjectionAlias(_))
      case _ =>
    }

  ensureProjectionAlias(projection)

  /**
   * Executes a query, opens a JDBC `ResultSet` and executes specified `actions`.
   */
  def resultSet[A](actions: ResultSet => A): A = tx.execute(toSql) { st =>
    setParams(st, 1)
    autoClose(st.executeQuery)(rs => actions(rs)) { throw _ }
  } { throw _ }

  /**
   * Uses the query projection to read specified `ResultSet`.
   */
  def read(rs: ResultSet): Option[T] = projection.read(rs)

  /**
   * Executes a query and returns `Seq[T]`, where `T` is designated by query `projection`.
   */
  def list(): Seq[T] = resultSet { rs =>
    var result = List[T]()
    while (rs.next) read(rs) match {
      case Some(r) =>
        result ++= List(r)
      case _ =>
    }
    return result
  }

  /**
   * Executes a query and returns a unique result.
   *
   * An exception is thrown if `ResultSet` yields more than one row.
   */
  def unique(): Option[T] = resultSet(rs => {
    if (!rs.next) return None
    else if (rs.isLast) return read(rs)
    else throw new ORMException("Unique result expected, but multiple rows found.")
  })

}

