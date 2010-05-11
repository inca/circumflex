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
 * A conrtact for SQL queries (data-retrieval). Specified `projection`
 * will be rendered in `SELECT` clause and will be used to read `ResultSet`.
 */
abstract class SQLQuery[T](val projection: Projection[T]) extends Query {

  /**
   * The `SELECT` clause of query. In normal circumstances this list should
   * only consist of single `projection` element; but if `GROUP_BY` clause
   * specifies projections that are not part of `projection`, than, they
   * are added here explicitly.
   */
  def projections: Seq[Projection[_]] = List(projection)

  /**
   * Make sure that projections with alias `this` are assigned query-unique alias.
   */
  protected def ensureProjectionAlias[T](projection: Projection[T]): Unit =
    projection match {
      case p: AtomicProjection[_] if (p.alias == "this") => p.as(nextAlias)
      case p: CompositeProjection[_] =>
        p.subProjections.foreach(ensureProjectionAlias(_))
      case _ =>
    }

  ensureProjectionAlias(projection)

  // ### Data Retrieval Stuff

  /**
   * Execute a query, open a JDBC `ResultSet` and executes specified `actions`.
   */
  def resultSet[A](actions: ResultSet => A): A = transactionManager.sql(toSql)(st => {
    sqlLog.debug(toSql)
    setParams(st, 1)
    auto(st.executeQuery)(actions)
  })

  // ### Executors

  /**
   * Use the query projection to read 
   */
  def read(rs: ResultSet): T = projection.read(rs)

  /**
   * Execute a query and return `Seq[T]`, where `T` is designated by query projection.
   */
  def list(): Seq[T] = resultSet(rs => {
    val result = new ListBuffer[T]()
    while (rs.next)
      result += read(rs)
    return result
  })

  /**
   * Execute a query and return a unique result.
   *
   * An exception is thrown if result set yields more than one row.
   */
  def unique(): Option[T] = resultSet(rs => {
    if (!rs.next) return None
    else if (rs.isLast) return Some(read(rs))
    else throw new ORMException("Unique result expected, but multiple rows found.")
  })

}

// ## Native SQL

class NativeSQLQuery[T](projection: Projection[T],
                        expression: ParameterizedExpression)
    extends SQLQuery[T](projection) {
  def parameters = expression.parameters
  def toSql = expression.toSql.replaceAll("\\{\\*\\}", projection.toSql)
}

// ## Full Select

/**
 * A full-fledged `SELECT` query.
 */
class Select[T](projection: Projection[T]) extends SQLQuery[T](projection) {

    // ### Commons

  protected var _auxProjections: Seq[Projection[_]] = Nil
  protected var _relations: Seq[RelationNode[_]] = Nil
  protected var _where: Predicate = EmptyPredicate
  protected var _having: Predicate = EmptyPredicate
  protected var _groupBy: Seq[Projection[_]] = Nil
  protected var _setOps: Seq[Pair[SetOperation, SQLQuery[T]]] = Nil
  protected var _orders: Seq[Order] = Nil
  protected var _limit: Int = -1
  protected var _offset: Int = 0

  /**
   * Query parameters.
   */
  def parameters: Seq[Any] = _where.parameters ++
      _having.parameters ++
      _setOps.flatMap(p => p._2.parameters) ++
      _orders.flatMap(_.parameters)

  /**
   * Queries combined with this subselect using specific set operation
   * (in pair, `SetOperation -> Subselect`),
   */
  def setOps = _setOps

  /**
   * The `SELECT` clause of query.
   */
  override def projections = List(projection) ++ _auxProjections

  // ### FROM clause

  def from = _relations

  /**
   * Applies specified `nodes` as this query's `FROM` clause.
   * All nodes with `this` alias are assigned query-unique alias.
   */
  def from(nodes: RelationNode[_]*): Select[T] = {
    this._relations = nodes.toList
    from.foreach(ensureNodeAlias(_))
    return this
  }
  def FROM(nodes: RelationNode[_]*): Select[T] = from(nodes: _*)

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

  def where(predicate: Predicate): Select[T] = {
    this._where = predicate
    return this
  }
  def WHERE(predicate: Predicate): Select[T] = where(predicate)

  /**
   * Use specified `expression` as the `WHERE` clause of this query
   * with specified named `params`.
   */
  def where(expression: String, params: Pair[String,Any]*): Select[T] =
    where(prepareExpr(expression, params: _*))
  def WHERE(expression: String, params: Pair[String,Any]*): Select[T] =
    where(expression, params: _*)

  // ### HAVING clause

  def having: Predicate = this._having

  def having(predicate: Predicate): Select[T] = {
    this._having = predicate
    return this
  }
  def HAVING(predicate: Predicate): Select[T] = having(predicate)

  /**
   * Use specified `expression` as the `HAVING` clause of this query
   * with specified named `params`.
   */
  def having(expression: String, params: Pair[String,Any]*): Select[T] =
    having(prepareExpr(expression, params: _*))
  def HAVING(expression: String, params: Pair[String,Any]*): Select[T] =
    having(expression, params: _*)

  // ### GROUP BY clause

  def groupBy: Seq[Projection[_]] = _groupBy

  def groupBy(proj: Projection[_]*): Select[T] = {
    proj.toList.foreach(p => addGroupByProjection(p))
    return this
  }
  def GROUP_BY(proj: Projection[_]*) = groupBy(proj: _*)

  protected def addGroupByProjection(proj: Projection[_]): Unit =
    findProjection(projection, p => p.equals(proj)) match {
      case None =>
        ensureProjectionAlias(proj)
        this._auxProjections ++= List(proj)
        this._groupBy ++= List(proj)
      case Some(p) => this._groupBy ++= List(p)
    }

  /**
   * Search deeply for a projection that matches specified `predicate` function.
   */
  protected def findProjection(projection: Projection[_],
                               predicate: Projection[_] => Boolean): Option[Projection[_]] =
    if (predicate(projection)) return Some(projection)
    else projection match {
      case p: CompositeProjection[_] =>
        return p.subProjections.find(predicate)
      case _ => return None
    }

  // ### Set Operations

  protected def addSetOp(op: SetOperation, sql: SQLQuery[T]): Select[T] = {
    _setOps ++= List(op -> sql)
    return this
  }

  def union(sql: SQLQuery[T]): Select[T] =
    addSetOp(OP_UNION, sql)
  def UNION(sql: SQLQuery[T]) = union(sql)

  def unionAll(sql: SQLQuery[T]): Select[T] =
    addSetOp(OP_UNION_ALL, sql)
  def UNION_ALL(sql: SQLQuery[T]) =
    unionAll(sql)

  def except(sql: SQLQuery[T]): Select[T] =
    addSetOp(OP_EXCEPT, sql)
  def EXCEPT(sql: SQLQuery[T]) =
    except(sql)

  def exceptAll(sql: SQLQuery[T]): Select[T] =
    addSetOp(OP_EXCEPT_ALL, sql)
  def EXCEPT_ALL(sql: SQLQuery[T]) =
    exceptAll(sql)

  def intersect(sql: SQLQuery[T]): Select[T] =
    addSetOp(OP_INTERSECT, sql)
  def INTERSECT(sql: SQLQuery[T]) =
    intersect(sql)

  def intersectAll(sql: SQLQuery[T]): Select[T] =
    addSetOp(OP_INTERSECT_ALL, sql)
  def INTERSECT_ALL(sql: SQLQuery[T]) =
    intersectAll(sql)

  // ### ORDER BY clause

  def orderBy = _orders
  def orderBy(order: Order*): Select[T] = {
    this._orders ++= order.toList
    return this
  }
  def ORDER_BY(order: Order*) =
    orderBy(order: _*)

  // ### LIMIT and OFFSET clauses

  def limit = this._limit
  def limit(value: Int): Select[T] = {
    _limit = value
    return this
  }
  def LIMIT(value: Int) = limit(value)

  def offset = this._offset
  def offset(value: Int): Select[T] = {
    _offset = value
    return this
  }
  def OFFSET(value: Int) = offset(value)

  // ### Miscellaneous

  def toSql = dialect.select(this)

}

// ## DML Queries

/**
 * A conrtact for DML queries (data-manipulation).
 */
trait DMLQuery extends Query {

  /**
   * Execute a query and return the number of affected rows.
   */
  def execute(): Int = transactionManager.dml(conn => {
    val sql = toSql
    sqlLog.debug(sql)
    auto(conn.prepareStatement(sql))(st => {
      setParams(st, 1)
      st.executeUpdate
    })
  })
}

// ## Native DML

class NativeDMLQuery(expression: ParameterizedExpression) extends DMLQuery {
  def parameters = expression.parameters
  def toSql = expression.toSql
}

// ## INSERT-SELECT query

/**
 * Functionality for INSERT-SELECT query. Data extracted using specified `query`
 * and inserted into specified `relation`.
 *
 * The projections of `query` must match the columns of target `relation`.
 */
class InsertSelect[R <: Record[R]](val relation: Relation[R],
                                   val query: SQLQuery[_])
    extends DMLQuery {
  if (relation.readOnly_?)
    throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")
  def parameters = query.parameters
  def toSql: String = dialect.insertSelect(this)
}

/**
 * A lil helper to keep stuff DSL'ly.
 */
class InsertSelectHelper[R <: Record[R]](val relation: Relation[R]) {
  def select[T](projection: Projection[T]) = new InsertSelect(relation, new Select(projection))
  def SELECT[T](projection: Projection[T]) = select(projection)
}

// ## DELETE query

/**
 * Functionality for DELETE query.
 */
class Delete[R <: Record[R]](val node: RelationNode[R])
    extends DMLQuery {
  val relation = node.relation
  if (relation.readOnly_?)
    throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")

  // ### WHERE clause

  protected var _where: Predicate = EmptyPredicate
  def where: Predicate = this._where
  def where(predicate: Predicate): Delete[R] = {
    this._where = predicate
    return this
  }
  def WHERE(predicate: Predicate) = where(predicate)

  // ### Miscellaneous
  def parameters = _where.parameters
  def toSql: String = dialect.delete(this)
}

// ## UPDATE query

/**
 * Functionality for UPDATE query.
 */
class Update[R <: Record[R]](val relation: Relation[R])
    extends DMLQuery {
  if (relation.readOnly_?)
    throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")

  // ### SET clause

  private var _setClause: Seq[Pair[Field[_], Any]] = Nil
  def setClause = _setClause
  def set[T](field: Field[T], value: T): Update[R] = {
    _setClause ++= List(field -> value)
    return this
  }
  def SET[T](field: Field[T], value: T): Update[R] = set(field, value)
  def set[P <: Record[P]](association: Association[R, P], value: P): Update[R]=
    set(association.field, value.id.get)
  def SET[P <: Record[P]](association: Association[R, P], value: P): Update[R] =
    set(association, value)
  def setNull[T](field: Field[T]): Update[R] = set(field, null.asInstanceOf[T])
  def SET_NULL[T](field: Field[T]): Update[R] = setNull(field)
  def setNull[P <: Record[P]](association: Association[R, P]): Update[R] =
    setNull(association.field)
  def SET_NULL[P <: Record[P]](association: Association[R, P]): Update[R] =
    setNull(association)

  // ### WHERE clause

  protected var _where: Predicate = EmptyPredicate
  def where: Predicate = this._where
  def where(predicate: Predicate): Update[R] = {
    this._where = predicate
    return this
  }
  def WHERE(predicate: Predicate) = where(predicate)

  // ### Miscellaneous

  def parameters = _setClause.map(_._2) ++ _where.parameters
  def toSql: String = dialect.update(this)

}



