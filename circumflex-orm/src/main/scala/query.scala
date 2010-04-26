package ru.circumflex.orm

import ORM._
import JDBC._
import java.sql.{ResultSet, PreparedStatement}
import collection.mutable.ListBuffer

// ## Query Commons

/**
 * The most common contract for queries.
 */
trait Query extends SQLable with ParameterizedExpression {
  protected var aliasCounter = 0;

  /**
   * Generate an alias to eliminate duplicates within query.
   */
  protected def nextAlias: String = {
    aliasCounter += 1
    return "this_" + aliasCounter
  }

  /**
   * Set prepared statement parameters of this query starting from specified index.
   * Because `Query` objects can be nested, this method should return the new starting
   * index of prepared statement parameter.
   */
  def setParams(st: PreparedStatement, startIndex: Int): Int = {
    var paramsCounter = startIndex;
    parameters.foreach(p => {
      typeConverter.write(st, p, paramsCounter)
      paramsCounter += 1
    })
    return paramsCounter
  }

  override def toString = toSql
}

// ## SQL Queries

/**
 * A conrtact for SQL queries (data-retrieval).
 */
trait SQLQuery extends Query {

  // ### Projections

  protected var _projections: Seq[Projection[_]] = Nil

  /**
   * The `SELECT` clause of this query.
   */
  def projections: Seq[Projection[_]] = _projections

  /**
   * Clear out the `SELECT` clause of this query.
   */
  def clearProjections: this.type = {
    this._projections = Nil
    return this
  }

  /**
   * Add specified projections to `SELECT` clause.
   * All projections with `this` alias are assigned query-unique alias.
   */
  def addProjection(projections: Projection[_]*): this.type = {
    projections.toList.foreach(ensureProjectionAlias(_))
    this._projections ++= projections
    return this
  }

  /**
   * Ensure that projections with alias `this` are assigned query-unique alias.
   */
  protected def ensureProjectionAlias[T](projection: Projection[T]): Unit =
    projection match {
      case p: AtomicProjection[_] if (p.alias == "this") => p.as(nextAlias)
      case p: CompositeProjection[_] =>
        p.subProjections.foreach(ensureProjectionAlias(_))
      case _ =>
    }

  /**
   * Find specified `projection` in specified `projectionsList` (search deeply).
   */
  protected def findProjection(projection: Projection[_],
                               projectionsList: Seq[Projection[_]]): Option[Projection[_]] = {
    if (projectionsList == Nil) return None
    projectionsList.find(p => p == projection) match {
      case None => findProjection(projection, projectionsList.flatMap {
        case p: CompositeProjection[_] => p.subProjections
        case _ => Nil
      })
      case value => value
    }
  }

  /**
   * Render the `SELECT` clause.
   */
  def sqlProjections = projections.toList.map(_.toSql).mkString(", ")

  // ### Data Retrieval Stuff

  /**
   * Execute a query, open a JDBC `ResultSet` and executes provided actions.
   */
  def resultSet[A](actions: ResultSet => A): A = transactionManager.sql(toSql)(st => {
    sqlLog.debug(toSql)
    setParams(st, 1)
    auto(st.executeQuery)(actions)
  })

  /**
   * Read a tuple from specified `ResultSet` using the projections of this query.
   */
  def readTuple(rs: ResultSet): Array[Any] =
    projections.map(_.read(rs).getOrElse(null)).toArray

  // ### Executors

  /**
   * Execute a query and return a list of tuples, designated by query projections.
   */
  def list(): Seq[Array[Any]] = resultSet(rs => {
    val result = new ListBuffer[Array[Any]]()
    while (rs.next)
      result += readTuple(rs)
    return result
  })

  /**
   * Execute a query and return a unique result.
   *
   * An exception is thrown if result set yields more than one row.
   */
  def unique(): Option[Array[Any]] = resultSet(rs => {
    if (!rs.next) return None
    else if (rs.isLast) return Some(projections.map(_.read(rs).getOrElse(null)).toArray)
    else throw new ORMException("Unique result expected, but multiple rows found.")
  })

}

// ## Native SQL

class NativeSQLQuery(expression: ParameterizedExpression) extends SQLQuery {
  def parameters = expression.parameters
  def toSql = expression.toSql.replaceAll("\\{\\*\\}", sqlProjections)
}

// ## Subselect

/**
 * A subset of `SELECT` query -- the one that can participate in subqueries,
 * it does not support `ORDER BY`, `LIMIT` and `OFFSET` clauses.
 */
class Subselect extends SQLQuery {

  // ### Commons

  protected var _relations: Seq[RelationNode[_]] = Nil
  protected var _where: Predicate = EmptyPredicate
  protected var _having: Predicate = EmptyPredicate
  protected var _groupBy: Seq[Projection[_]] = Nil
  protected var _setOps: Seq[Pair[SetOperation, Subselect]] = Nil

  def this(nodes: RelationNode[_]*) = {
    this()
    nodes.foreach(addFrom(_))
  }

  /**
   * Query parameters.
   */
  def parameters: Seq[Any] = _where.parameters ++
          _having.parameters ++
          _setOps.flatMap(p => p._2.parameters)

  /**
   * Queries combined with this subselect using specific set operation
   * (in pair, `SetOperation -> Subselect`),
   */
  def setOps = _setOps

  // ### FROM clause

  def relations = _relations

  /**
   * Add specified `node` to `FROM` clause.
   * All nodes with `this` alias are assigned query-unique alias.
   * All projections are added too.
   */
  def addFrom(node: RelationNode[_]): this.type = {
    ensureNodeAlias(node)
    this._relations ++= List[RelationNode[_]](node)
    addProjection(node.projections: _*)
    return this
  }

  protected def ensureNodeAlias(node: RelationNode[_]): RelationNode[_] =
    node match {
      case j: JoinNode[_, _] =>
        ensureNodeAlias(j.left)
        ensureNodeAlias(j.right)
        j
      case n: RelationNode[_] if (n.alias == "this") => node.as(nextAlias)
      case n => n
    }

  // ### WHERE clause

  def where: Predicate = this._where

  def where(predicate: Predicate): this.type = {
    this._where = predicate
    return this
  }
  def WHERE(predicate: Predicate): this.type = where(predicate)

  /**
   * Use specified `expression` as the `WHERE` clause of this query
   * with specified named `params`.
   */
  def where(expression: String, params: Pair[String,Any]*): this.type =
    where(prepareExpr(expression, params: _*))
  def WHERE(expression: String, params: Pair[String,Any]*): this.type =
    where(expression, params: _*)

  // ### HAVING clause

  def having: Predicate = this._having

  def having(predicate: Predicate): this.type = {
    this._having = predicate
    return this
  }
  def HAVING(predicate: Predicate): this.type = having(predicate)

  /**
   * Use specified `expression` as the `HAVING` clause of this query
   * with specified named `params`.
   */
  def having(expression: String, params: Pair[String,Any]*): this.type =
    having(prepareExpr(expression, params: _*))
  def HAVING(expression: String, params: Pair[String,Any]*): this.type =
    having(expression, params: _*)

  // ### GROUP BY clause

  def groupBy: Seq[Projection[_]] = {
    var result = _groupBy
    if (projections.exists(_.grouping_?))
      projections.filter(!_.grouping_?)
              .foreach(p => if (!result.contains(p)) result ++= List(p))
    return result
  }

  def groupBy(proj: Projection[_]*): this.type = {
    proj.toList.foreach(p => addGroupByProjection(p))
    return this
  }
  def GROUP_BY(proj: Projection[_]*): this.type = groupBy(proj: _*)

  protected def addGroupByProjection(proj: Projection[_]): this.type = {
    val pr = findProjection(proj, _projections) match {
      case Some(p) => p
      case _ => {
        addProjection(proj)
        proj
      }
    }
    this._groupBy ++= List[Projection[_]](pr)
    return this
  }

  // ### Set Operations

  protected def addSetOp(op: SetOperation, subselect: Subselect): this.type = {
    _setOps ++= List(op -> subselect)
    return this
  }

  def union(subselect: Subselect): this.type =
    addSetOp(OP_UNION, subselect)
  def UNION(subselect: Subselect): this.type = union(subselect)

  def unionAll(subselect: Subselect): this.type =
    addSetOp(OP_UNION_ALL, subselect)
  def UNION_ALL(subselect: Subselect): this.type =
    unionAll(subselect)

  def except(subselect: Subselect): this.type =
    addSetOp(OP_EXCEPT, subselect)
  def EXCEPT(subselect: Subselect): this.type =
    except(subselect)

  def exceptAll(subselect: Subselect): this.type =
    addSetOp(OP_EXCEPT_ALL, subselect)
  def EXCEPT_ALL(subselect: Subselect): this.type =
    exceptAll(subselect)

  def intersect(subselect: Subselect): this.type =
    addSetOp(OP_INTERSECT, subselect)
  def INTERSECT(subselect: Subselect): this.type =
    intersect(subselect)

  def intersectAll(subselect: Subselect): this.type =
    addSetOp(OP_INTERSECT_ALL, subselect)
  def INTERSECT_ALL(subselect: Subselect): this.type =
    intersectAll(subselect)

  // ### Miscellaneous

  def toSubselectSql = dialect.subselect(this)

  def toSql = toSubselectSql
}

// ## Full Select

/**
 * A full-fledged `SELECT` query.
 */
class Select extends Subselect {

  protected var _orders: Seq[Order] = Nil
  protected var _limit: Int = -1
  protected var _offset: Int = 0

  def this(nodes: RelationNode[_]*) = {
    this()
    nodes.toList.foreach(addFrom(_))
  }

  override def parameters: Seq[Any] =
    super.parameters ++ _orders.flatMap(_.parameters)

  // ### ORDER BY clause

  def orderBy = _orders
  def orderBy(order: Order*): this.type = {
    this._orders ++= order.toList
    return this
  }
  def ORDER_BY(order: Order*): this.type =
    orderBy(order: _*)

  // ### LIMIT and OFFSET clauses

  def limit = this._limit
  def limit(value: Int): this.type = {
    _limit = value
    return this
  }
  def LIMIT(value: Int): this.type = limit(value)

  def offset = this._offset
  def offset(value: Int): this.type = {
    _offset = value
    return this
  }
  def OFFSET(value: Int): this.type = offset(value)

  // ### Miscellaneous

  override def toSql = dialect.select(this)

}

/**
 * A helper for query DSLs.
 */
class SelectHelper(val projections: Seq[Projection[_]]) {
  def from(nodes: RelationNode[_]*): Select = {
    val q = new Select(nodes: _*)
    if (projections.size > 0) {
      q.clearProjections
      q.addProjection(projections: _*)
    }
    return q
  }
  def FROM(nodes: RelationNode[_]*): Select = from(nodes: _*)
}

