package ru.circumflex.orm

import java.sql.{ResultSet, PreparedStatement}
import ru.circumflex.core._

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

  // Keep track of last execution time
  protected var _executionTime = 0l
  def executionTime = _executionTime

  protected var _aliasCounter = 0;

  /**
   * Generates an alias to eliminate duplicates within query.
   */
  protected def nextAlias: String = {
    _aliasCounter += 1
    return "this_" + _aliasCounter
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
  def resultSet[A](actions: ResultSet => A): A = {
    val result = time {
      tx.execute(toSql) { st =>
        setParams(st, 1)
        val rs = st.executeQuery
        try {
          actions(rs)
        } finally {
          rs.close
        }
      } { throw _ }
    }
    _executionTime = result._1
    Statistics.executeSql(this)
    return result._2
  }

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
    result
  }

  /**
   * Executes a query and returns a unique result.
   *
   * An exception is thrown if `ResultSet` yields more than one row.
   */
  def unique(): Option[T] = resultSet(rs => {
    if (!rs.next)
      None
    else if (rs.isLast)
      read(rs)
    else throw new ORMException("Unique result expected, but multiple rows found.")
  })

}

class NativeSQLQuery[T](projection: Projection[T],
                        expression: ParameterizedExpression)
    extends SQLQuery[T](projection) {
  def parameters = expression.parameters
  def toSql = expression.toSql.replaceAll("\\{\\*\\}", projection.toSql)
}

class Select[T](projection: Projection[T]) extends SQLQuery[T](projection) {

  // Commons

  protected var _distinct: Boolean = false
  protected var _auxProjections: Seq[Projection[_]] = Nil
  protected var _relations: Seq[RelationNode[_, _]] = Nil
  protected var _where: Predicate = EmptyPredicate
  protected var _having: Predicate = EmptyPredicate
  protected var _groupBy: Seq[Projection[_]] = Nil
  protected var _setOps: Seq[Pair[SetOperation, SQLQuery[T]]] = Nil
  protected var _orders: Seq[Order] = Nil
  protected var _limit: Int = -1
  protected var _offset: Int = 0

  def parameters: Seq[Any] = _where.parameters ++
      _having.parameters ++
      _setOps.flatMap(p => p._2.parameters) ++
      _orders.flatMap(_.parameters)

  def setOps = _setOps

  // SELECT clause

  override def projections = List(projection) ++ _auxProjections

  def distinct_?(): Boolean = _distinct
  def DISTINCT(): Select[T] = {
    this._distinct = true
    return this
  }

  // FROM clause

  def from = _relations
  def FROM(nodes: RelationNode[_, _]*): Select[T] = {
    this._relations = nodes.toList
    from.foreach(ensureNodeAlias(_))
    return this
  }

  protected def ensureNodeAlias(node: RelationNode[_, _]): RelationNode[_, _] =
    node match {
      case j: JoinNode[_, _, _, _] =>
        ensureNodeAlias(j.left)
        ensureNodeAlias(j.right)
        j
      case n: RelationNode[_, _] if (n.alias == "this") => node.AS(nextAlias)
      case n => n
    }

  // WHERE clause

  def where: Predicate = this._where
  def WHERE(predicate: Predicate): Select[T] = {
    this._where = predicate
    return this
  }
  def WHERE(expression: String, params: Pair[String,Any]*): Select[T] =
    WHERE(prepareExpr(expression, params: _*))

  // HAVING clause

  def having: Predicate = this._having
  def HAVING(predicate: Predicate): Select[T] = {
    this._having = predicate
    return this
  }
  def HAVING(expression: String, params: Pair[String,Any]*): Select[T] =
    HAVING(prepareExpr(expression, params: _*))

  // GROUP BY clause

  def groupBy: Seq[Projection[_]] = _groupBy

  def GROUP_BY(proj: Projection[_]*): Select[T] = {
    proj.toList.foreach(p => addGroupByProjection(p))
    return this
  }

  protected def addGroupByProjection(proj: Projection[_]): Unit =
    findProjection(projection, p => p.equals(proj)) match {
      case None =>
        ensureProjectionAlias(proj)
        this._auxProjections ++= List(proj)
        this._groupBy ++= List(proj)
      case Some(p) => this._groupBy ++= List(p)
    }

  /**
   * Searches deeply for a `projection` that matches specified `predicate` function.
   */
  protected def findProjection(projection: Projection[_],
                               predicate: Projection[_] => Boolean): Option[Projection[_]] =
    if (predicate(projection)) return Some(projection)
    else projection match {
      case p: CompositeProjection[_] =>
        return p.subProjections.find(predicate)
      case _ => return None
    }

  // Set Operations

  protected def addSetOp(op: SetOperation, sql: SQLQuery[T]): Select[T] = {
    val q = clone()
    q._setOps ++= List(op -> sql)
    return q
  }

  def UNION(sql: SQLQuery[T]): Select[T] =
    addSetOp(OP_UNION, sql)
  def UNION_ALL(sql: SQLQuery[T]): Select[T] =
    addSetOp(OP_UNION_ALL, sql)
  def EXCEPT(sql: SQLQuery[T]): Select[T] =
    addSetOp(OP_EXCEPT, sql)
  def EXCEPT_ALL(sql: SQLQuery[T]): Select[T] =
    addSetOp(OP_EXCEPT_ALL, sql)
  def INTERSECT(sql: SQLQuery[T]): Select[T] =
    addSetOp(OP_INTERSECT, sql)
  def INTERSECT_ALL(sql: SQLQuery[T]): Select[T] =
    addSetOp(OP_INTERSECT_ALL, sql)

  // ORDER BY clause

  def orderBy = _orders
  def ORDER_BY(order: Order*): Select[T] = {
    this._orders ++= order.toList
    return this
  }

  // LIMIT and OFFSET clauses

  def limit = this._limit
  def LIMIT(value: Int): Select[T] = {
    _limit = value
    return this
  }

  def offset = this._offset
  def OFFSET(value: Int): Select[T] = {
    _offset = value
    return this
  }

  // Miscellaneous

  def toSql = dialect.select(this)

}

/*! The `DMLQuery` trait defines a contract for data-manipulation queries. */
trait DMLQuery extends Query {

  /**
   * Executes a query and returns the number of affected rows.
   */
  def execute(): Int = {
    val result = time {
      tx.execute(toSql){ st =>
        setParams(st, 1)
        st.executeUpdate
      } { throw _ }
    }
    _executionTime = result._1
    Statistics.executeDml(this)
    return result._2
  }
}

class NativeDMLQuery(expression: ParameterizedExpression) extends DMLQuery {
  def parameters = expression.parameters
  def toSql = expression.toSql
}

class Insert[PK, R <: Record[PK, R]](val relation: Relation[PK, R],
                                     val fields: Seq[Field[_, R]])
    extends DMLQuery {
  def parameters = fields.map(_.value)
  def toSql: String = dialect.insert(this)
}

/**
 * Provides functionality for `INSERT ... SELECT` queries. Data is extracted using
 * specified `query` and is inserted into specified `relation`.
 *
 * The projections of `query` should match the columns of target `relation`.
 */
class InsertSelect[PK, R <: Record[PK, R]](val relation: Relation[PK, R],
                                           val query: SQLQuery[_])
    extends DMLQuery {
  if (relation.readOnly_?)
    throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")
  def parameters = query.parameters
  def toSql: String = dialect.insertSelect(this)
}

class InsertSelectHelper[PK, R <: Record[PK, R]](val relation: Relation[PK, R]) {
  def SELECT[T](projection: Projection[T]) = new InsertSelect(relation, new Select(projection))
}

/**
 * Provides functionality for `DELETE` queries.
 */
class Delete[PK, R <: Record[PK, R]](val node: RelationNode[PK, R])
    extends DMLQuery {
  val relation = node.relation
  if (relation.readOnly_?)
    throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")

  protected var _where: Predicate = EmptyPredicate
  def where: Predicate = this._where
  def WHERE(predicate: Predicate): Delete[PK, R] = {
    this._where = predicate
    return this
  }

  def parameters = _where.parameters
  def toSql: String = dialect.delete(this)
}

/**
 * Provides functionality for `UPDATE` queries.
 */
class Update[PK, R <: Record[PK, R]](val node: RelationNode[PK, R])
    extends DMLQuery {
  val relation = node.relation
  if (relation.readOnly_?)
    throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")

  private var _setClause: Seq[(Field[_, R], Option[Any])] = Nil
  def setClause = _setClause
  def SET[T](field: Field[T, R], value: T): Update[PK, R] = {
    _setClause ++= List(field -> Some(value))
    return this
  }
  def SET[K, P <: Record[K, P]](association: Association[K, R, P], value: P): Update[PK, R]=
    SET(association.field.asInstanceOf[Field[Any, R]], value.PRIMARY_KEY.value)
  def SET_NULL[T](field: Field[T, R]): Update[PK, R] = {
    _setClause ++= List(field -> None)
    return this
  }
  def SET_NULL[K, P <: Record[K, P]](association: Association[K, R, P]): Update[PK, R] =
    SET_NULL(association.field)

  protected var _where: Predicate = EmptyPredicate
  def where: Predicate = this._where
  def WHERE(predicate: Predicate): Update[PK, R] = {
    this._where = predicate
    return this
  }

  def parameters = _setClause.map(_._2) ++ _where.parameters
  def toSql: String = dialect.update(this)

}