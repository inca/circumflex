package pro.savant.circumflex
package orm

import java.sql.{ResultSet, PreparedStatement}
import core._
import scala.collection.mutable.ListBuffer

/*!# Querying

SQL and DML queries form the heart of Circumflex ORM DSL.

Common features implemented in the `Query` trait are _named parameters_
which allow query reuse and _ensuring alias uniqueness_ which prevents
implicit relation node aliases from colliding within a single query.

The `SQLQuery` trait represents data-retrieval queries which usually employ
the `executeQuery` method of JDBC `PreparedStatement` and process JDBC
`ResultSet`.

The `DMLQuery` trait represents data-manipulation queries which usually
employ the `executeUpdate` method of JDBC `PreparedStatement` and return
the number of affected rows.
*/
trait Query extends SQLable with Expression with Cloneable {

  // Keep track of last execution time
  protected var _executionTime = 0l
  def executionTime = _executionTime

  protected var _aliasCounter = 0

  protected def nextAlias: String = {
    _aliasCounter += 1
    "this_" + _aliasCounter
  }

  // Logging

  protected def writeLog() {
    if (ORM_LOG.isDebug) {
      val msg = new StringBuilder
      msg.append(ormConf.prefix(": "))
      msg.append("[")
      if (_executionTime > 1000)
        msg.append(_executionTime / 1000 + "s")
      else msg.append(_executionTime + "ms")
      msg.append("] ")
      msg.append(toInlineSql)
      ORM_LOG.debug(msg.toString)
    }
  }

  // Named parameters

  def setParams(st: PreparedStatement, index: Int): Int = {
    var paramsCounter = index
    parameters.foreach { p =>
      ormConf.typeConverter.write(st, convertNamedParam(p), paramsCounter)
      paramsCounter += 1
    }
    paramsCounter
  }

  protected var _namedParams: Map[String, Any] = Map()

  def renderParams: Seq[Any] = parameters.map(p => convertNamedParam(p))

  def set(name: String, value: Any): this.type = {
    _namedParams += name -> value
    this
  }

  protected def convertNamedParam(param: Any): Any = param match {
    case s: Symbol => lookupNamedParam(s.name)
    case s: String if (s.startsWith(":")) =>
      lookupNamedParam(s)
    case _ => param
  }

  protected def lookupNamedParam(name: String): Any =
    _namedParams.get(name.replaceAll("^:", "")).getOrElse(name)

  override def clone(): this.type = super.clone.asInstanceOf[this.type]

  override def toString = toSql
}

/*! The `SQLQuery` trait defines a contract for data-retrieval queries.
It's only type parameter `T` designates the query result type (it is
determined by specified `projections`).
*/
abstract class SQLQuery[T](val projection: Projection[T]) extends Query {

  def isUndefinedQueryPlan = false

  // Projections

  def projections: Seq[Projection[_]] = List(projection)

  protected def ensureProjectionAlias[P](projection: Projection[P]) {
    projection match {
      case p: AtomicProjection[_] if (p.alias == "this") => p.AS(nextAlias)
      case p: CompositeProjection[_] =>
        p.subProjections.foreach(ensureProjectionAlias(_))
      case _ =>
    }
  }

  ensureProjectionAlias(projection)

  // Query execution

  def resultSet[A](actions: ResultSet => A): A = {
    if (isUndefinedQueryPlan)
      ORM_LOG.warn("Query plan contains ambiguous joins. " +
          "Please recheck the FROM clause.")
    val result = time {
      tx.execute(toSql, { st =>
        setParams(st, 1)
        val rs = st.executeQuery()
        try {
          actions(rs)
        } finally {
          rs.close()
        }
      }, { throw _ })
    }
    _executionTime = result._1
    writeLog()
    ormConf.statisticsManager.executeSql(this)
    result._2
  }

  def read(rs: ResultSet): Option[T] = projection.read(rs)

  def list(): Seq[T] = resultSet { rs =>
    var result = List[T]()
    while (rs.next) read(rs) match {
      case Some(r) =>
        result ++= List(r)
      case _ =>
    }
    result
  }

  def unique(): Option[T] = resultSet(rs => {
    if (!rs.next)
      None
    else if (rs.isLast)
      read(rs)
    else throw new ORMException("Unique result expected, but multiple rows found.")
  })

}

// Native SQL

class NativeSQLQuery[T](projection: Projection[T],
                        expression: Expression)
    extends SQLQuery[T](projection) {
  def parameters = expression.parameters
  def toSql = expression.toSql.replaceAll("\\{\\*\\}", projection.toSql)
}

/*! `SearchQuery` represents a query with a `WHERE` clause. */
trait SearchQuery extends Query {
  protected var _where: Predicate = EmptyPredicate

  def whereClause = this._where

  def WHERE(predicate: Predicate): this.type = {
    this._where = predicate
    this
  }

  def WHERE(expression: String, params: Pair[String,Any]*): this.type =
    WHERE(prepareExpr(expression, params: _*))

  def add(predicates: Predicate*): this.type = {
    whereClause match {
      case EmptyPredicate =>
        this._where = AND(predicates: _*)
      case p: AggregatePredicate if (p.operator == ormConf.dialect.AND) =>
        this._where = p.add(predicates: _*)
      case p =>
        this._where = AND(_where).add(predicates: _*).toPredicate
    }
    this
  }

  def add(expression: String, params: Pair[String, Any]*): this.type =
    add(prepareExpr(expression, params: _*))
}

class Select[T](projection: Projection[T])
    extends SQLQuery[T](projection)
    with SearchQuery {

  // Commons
  protected var _distinct: Boolean = false
  protected var _relations: Seq[RelationNode[_, _]] = Nil

  protected var _having: Predicate = EmptyPredicate
  protected var _groupBy: Seq[Projection[_]] = Nil

  protected var _orders: Seq[Order] = Nil
  protected var _limit: Int = -1
  protected var _offset: Int = 0

  def parameters: Seq[Any] = _where.parameters ++
      _having.parameters ++
      _setOps.flatMap(p => p._2.parameters) ++
      _orders.flatMap(_.parameters)

  // SELECT clause

  override def projections = List(projection)

  def isDistinct: Boolean = _distinct
  def DISTINCT: Select[T] = {
    this._distinct = true
    this
  }

  // FROM clause

  def fromClause = _relations
  def FROM(nodes: RelationNode[_, _]*): Select[T] = {
    this._relations = nodes.toList
    fromClause.foreach(ensureNodeAlias(_))
    this
  }

  override def isUndefinedQueryPlan = _relations.exists { node =>
    if (node.isInstanceOf[JoinNode[_, _, _, _]])
      node.asInstanceOf[JoinNode[_, _, _, _]].isUndefined
    else false
  }

  protected def ensureNodeAlias(node: RelationNode[_, _]): RelationNode[_, _] =
    if (node.isInstanceOf[JoinNode[_, _, _, _]]) {
      val j = node.asInstanceOf[JoinNode[_, _, _, _]]
      ensureNodeAlias(j.left)
      ensureNodeAlias(j.right)
      j
    } else if (node.alias == "this") {
      node.AS(nextAlias)
    } else node

  // HAVING clause

  def havingClause: Predicate = this._having

  def HAVING(predicate: Predicate): Select[T] = {
    this._having = predicate
    this
  }

  def HAVING(expression: String, params: Pair[String,Any]*): Select[T] =
    HAVING(prepareExpr(expression, params: _*))

  // GROUP BY clause

  protected var _groupByClause = ""

  def groupByClause = _groupByClause

  def GROUP_BY(proj: Projection[_]*): Select[T] = {
    proj.toList.foreach(p => addGroupByProjection(p))
    this
  }

  protected def addGroupByProjection(proj: Projection[_]) {
    findProjection(projection, p => p.equals(proj)) match {
      case None =>
        this.appendUnaliasedGroupBy(proj)
      case Some(p) =>
        this.appendGroupBy(p.sqlAliases.mkString(", "))
    }
  }

  protected def appendUnaliasedGroupBy(projection: Projection[_]) {
    projection match {
      case ap: AtomicProjection[_] => appendGroupBy(ap.expression)
      case cp: CompositeProjection[_] =>
        cp.subProjections.foreach(p => appendUnaliasedGroupBy(p))
      case _ =>
    }
  }

  protected def appendGroupBy(expr: String) {
    if (groupByClause == "") _groupByClause += expr
    else _groupByClause += ", " + expr
  }

  protected def findProjection
  (projection: Projection[_],
   predicate: Projection[_] => Boolean): Option[Projection[_]] =
    if (predicate(projection)) Some(projection)
    else projection match {
      case p: CompositeProjection[_] =>
        p.subProjections.find(predicate)
      case _ => None
    }

  // Set Operations

  protected var _setOps: Seq[Pair[SetOperation, SQLQuery[T]]] = Nil

  def setOps = _setOps

  protected def addSetOp(op: SetOperation, sql: SQLQuery[T]): Select[T] = {
    val q = clone()
    q._setOps ++= List(op -> sql)
    q
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

  def orderByClause = _orders

  def ORDER_BY(order: Order*): Select[T] = {
    this._orders ++= order.toList
    this
  }

  // LIMIT and OFFSET clauses

  def limit = this._limit

  def LIMIT(value: Int): Select[T] = {
    _limit = value
    this
  }

  def offset = this._offset

  def OFFSET(value: Int): Select[T] = {
    _offset = value
    this
  }

  // Miscellaneous

  def toSql = ormConf.dialect.select(this)

}

/*! The `DMLQuery` trait defines a contract for data-manipulation queries. */
trait DMLQuery extends Query {

  def execute(): Int = {
    if (ormConf.readOnly)
      throw new CircumflexException(
        "Read-only configuration does not allow DML statements.")
    val result = time {
      tx.execute(toSql, { st =>
        setParams(st, 1)
        st.executeUpdate()
      }, { throw _ })
    }
    _executionTime = result._1
    writeLog()
    ormConf.statisticsManager.executeDml(this)
    result._2
  }

}

class NativeDMLQuery(expression: Expression) extends DMLQuery {

  def parameters = expression.parameters

  def toSql = expression.toSql

}

class Insert[PK, R <: Record[PK, R]](val relation: Relation[PK, R],
                                     val fields: Seq[Field[_, R]])
    extends DMLQuery {

  def parameters = fields.map(_.value)

  def toSql: String = ormConf.dialect.insert(this)

}

class InsertSelect[PK, R <: Record[PK, R]](val relation: Relation[PK, R],
                                           val query: SQLQuery[_])
    extends DMLQuery {

  if (relation.isReadOnly)
    throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")

  def parameters = query.parameters

  def toSql: String = ormConf.dialect.insertSelect(this)

}

class InsertSelectHelper[PK, R <: Record[PK, R]](val relation: Relation[PK, R]) {

  def SELECT[T](projection: Projection[T]) =
    new InsertSelect(relation, new Select(projection))

}

class Delete[PK, R <: Record[PK, R]](val node: RelationNode[PK, R])
    extends DMLQuery with SearchQuery {

  val relation = node.relation
  if (relation.isReadOnly)
    throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")

  def parameters = _where.parameters

  def toSql: String = ormConf.dialect.delete(this)

}

class Update[PK, R <: Record[PK, R]](val node: RelationNode[PK, R])
    extends DMLQuery with SearchQuery {

  val relation = node.relation
  if (relation.isReadOnly)
    throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")

  private var _setClause = new ListBuffer[String]
  private var _setParameters = new ListBuffer[Option[Any]]

  def setClause = _setClause.toSeq

  protected def addSet[T](field: Field[T, R], value: Option[T]): this.type = {
    _setClause += field.name + " = " + field.placeholder
    _setParameters += value
    this
  }

  def SET[T](field: Field[T, R], value: T): this.type =
    addSet(field, Some(value))

  def SET[K, P <: Record[K, P]](association: Association[K, R, P],
                                value: P): this.type =
    if (value.isTransient) SET_NULL(association)
    else SET(association.field.asInstanceOf[Field[Any, R]], value.PRIMARY_KEY.value)

  def SET_NULL[T](field: Field[T, R]): this.type =
    addSet(field, None)

  def SET_NULL[K, P <: Record[K, P]](association: Association[K, R, P]): this.type =
    SET_NULL(association.field)

  def SET(expr: String, params: Any*): this.type = {
    _setClause += expr
    _setParameters ++= params.map(v => any2option(v))
    this
  }

  def parameters = _setParameters.toSeq ++ _where.parameters

  def toSql = ormConf.dialect.update(this)

}

/*!# Native Queries DSL

You can construct Native query from any `String` using either `toSql` or
`toDml` methods, which are pimped onto `String` by implicit conversion
into `NativeQueryHelper`. Use `toSql` for selection queries and `toDml`
for manipulation queries.

In case of `toSql` method you should provide a `Projection` parameterized
with target query type.

Here are some examples. Let's take a look at querying first.

``` {.scala}
// First, determine query result type (the SELECT clause):
val p = expr[String]("c.name")
// Second, convert String to Native SQL using specified projection:
val q = "SELECT {*} FROM orm.country c WHERE c.code LIKE :code".toSql(p)
// Now execute the query, using specified parameters
q.set("code", "ch").unique.get must_== "Switzerland"
// Note that named parameters allow reusing queries
q.set("code", "ru").unique.get must_== "Russia"
```

And now let's take a look at the manipulation example.

``` {.scala}
// It's that easy:
"UPDATE orm.country c SET c.code = c.code".toDml.execute()
```
*/
class NativeQueryHelper(val expr: String) {

  def toSql[T](projection: Projection[T]): NativeSQLQuery[T] =
    new NativeSQLQuery[T](projection, prepareExpr(expr))

  def toDml: NativeDMLQuery = new NativeDMLQuery(prepareExpr(expr))

}