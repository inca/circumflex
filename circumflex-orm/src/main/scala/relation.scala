package ru.circumflex
package orm

import core._
import java.lang.reflect.Method

/*!# Relations

Like records, relations are alpha and omega of relational theory and, therefore,
of Circumflex ORM API.

In relational model a relation is a data structure which consists of a heading and
an unordered set of rows which share the same type. Classic relational databases
often support two type of relations, [tables](#table) and [views](#view).

In Circumflex ORM the relation contains record metadata and various operational
information. There should be only one relation instance per application, so
by convention the relations should be the companion objects of corresponding
records:

    // record definition
    class Country extends Record[String, Country] {
      val code = "code".VARCHAR(2).NOT_NULL
      val name = "name".TEXT.NOT_NULL

      def PRIMARY_KEY = code
      def relation = Country
    }

    // corresponding relation definition
    object Country extends Country with Table[String, Country] {

    }

The relation should also inherit the structure of corresponding record so that
it could be used to compose predicates and other expressions in a DSL-style.
*/
trait Relation[PK, R <: Record[PK, R]]
    extends Record[PK, R] with SchemaObject { this: R =>

  protected var _initialized = false

  /*!## Commons

 If the relation follows default conventions of Circumflex ORM (about
 companion objects), then record class is inferred automatically. Otherwise
 you should override the `recordClass` method.
  */
  val _recordClass: Class[R] = Class.forName(
    this.getClass.getName.replaceAll("\\$(?=\\Z)", ""),
    true,
    Thread.currentThread.getContextClassLoader
  ).asInstanceOf[Class[R]]
  def recordClass: Class[R] = _recordClass

  /*! By default the relation name is inferred from `recordClass` by replacing
  camelcase delimiters with underscores (for example, record with class
  `ShoppingCartItem` will have a relation with name `shopping_cart_item`).
  You can override `relationName` to use different name.
  */
  val _relationName = camelCaseToUnderscore(recordClass.getSimpleName)
  def relationName = _relationName
  def qualifiedName = ormConf.dialect.relationQualifiedName(this)

  /*! Default schema name is configured via the `orm.defaultSchema` configuration property.
  You may provide different schema for different relations by overriding their `schema` method.
   */
  def schema: Schema = ormConf.defaultSchema

  /*! The `isReadOnly` method is used to indicate whether the DML operations
  are allowed with this relation. Tables usually allow them and views usually don't.
   */
  def isReadOnly: Boolean

  /*! The `isAutoRefresh` method is used to indicate whether the record should be immediately
  refreshed after every successful `INSERT` or `UPDATE` operation. By default it returns `false`
  to maximize performance. However, if the relation contains columns with auto-generated values
  (e.g. `DEFAULT` clauses, auto-increments, triggers, etc.) then you should override this method.
   */
  def isAutoRefresh: Boolean = false

  /*! Use the `AS` method to create a relation node from this relation with an explicit alias. */
  def AS(alias: String): RelationNode[PK, R] = new RelationNode(this).AS(alias)

  def findAssociation[T, F <: Record[T, F]](relation: Relation[T, F]): Option[Association[T, R, F]] =
    associations.find(_.parentRelation == relation)
        .asInstanceOf[Option[Association[T, R, F]]]

  val validation = new RecordValidator[PK, R]()

  /*!## Simple queries

  Following methods will help you perform common querying tasks:

    * `get` retrieves a record either from cache or from database by specified `id`;
    * `all` retrieves all records.
   */
  def get(id: PK): Option[R] =
    tx.cache.cacheRecord(id, this,
      (this.AS("root")).map(r => r.criteria.add(r.PRIMARY_KEY EQ id).unique()))

  def all: Seq[R] = this.AS("root").criteria.list()

  /*!## Metadata

  Relation metadata contains operational information about it's records by
  introspecting current instance upon initialization.
  */
  protected var _methodsMap: Map[Field[_, R], Method] = Map()
  def methodsMap: Map[Field[_, R], Method] = {
    init()
    _methodsMap
  }

  protected var _fields: List[Field[_, R]] = Nil
  def fields: Seq[Field[_, R]] = {
    init()
    _fields
  }

  protected var _associations: List[Association[_, R, _]] = Nil
  def associations: Seq[Association[_, R, _]] = {
    init()
    _associations
  }

  protected var _constraints: List[Constraint] = Nil
  def constraints: Seq[Constraint] = {
    init()
    _constraints
  }

  protected var _indexes: List[Index] = Nil
  def indexes: Seq[Index] = {
    init()
    _indexes
  }

  private def findMembers(cl: Class[_]) {
    if (cl != classOf[Any]) findMembers(cl.getSuperclass)
    cl.getDeclaredFields
        .flatMap(f => try Some(cl.getMethod(f.getName)) catch { case e: Exception => None })
        .foreach(processMember(_))
  }

  private def processMember(m: Method) {
    val cl = m.getReturnType
    if (classOf[ValueHolder[_, R]].isAssignableFrom(cl)) {
      val vh = m.invoke(this).asInstanceOf[ValueHolder[_, R]]
      processHolder(vh, m)
    } else if (classOf[Constraint].isAssignableFrom(cl)) {
      val c = m.invoke(this).asInstanceOf[Constraint]
      this._constraints ++= List(c)
    } else if (classOf[Index].isAssignableFrom(cl)) {
      val i = m.invoke(this).asInstanceOf[Index]
      this._indexes ++= List(i)
    }
  }

  private def processHolder(vh: ValueHolder[_, R], m: Method) {
    vh match {
      case f: Field[_, R] =>
        this._fields ++= List(f)
        if (f.isUnique) this.UNIQUE(f)
        this._methodsMap += (f -> m)
      case a: Association[_, R, _] =>
        this._associations ++= List[Association[_, R, _]](a)
        this._constraints ++= List(associationFK(a))
        processHolder(a.field, m)
      case _ =>
    }
  }

  private def associationFK(a: Association[_, R, _]) =
    CONSTRAINT(relationName + "_" + a.name + "_fkey")
        .FOREIGN_KEY(a.field)
        .REFERENCES(a.parentRelation, a.parentRelation.PRIMARY_KEY)
        .ON_DELETE(a.onDeleteClause)
        .ON_UPDATE(a.onUpdateClause)

  def init() {
    if (!_initialized) synchronized {
      if (!_initialized) try {
        findMembers(this.getClass)
        ormConf.dialect.initializeRelation(this)
        _fields.foreach(ormConf.dialect.initializeField(_))
        this._initialized = true
      } catch {
        case e: NullPointerException =>
          throw new ORMException("Failed to initialize " + relationName + ": " +
              "possible cyclic dependency between relations. " +
              "Make sure that at least one side uses weak reference to another " +
              "(change `val` to `lazy val` for fields and to `def` for inverse associations).", e)
        case e: Exception =>
          throw new ORMException("Failed to initialize " + relationName + ".", e)
      }
    }
  }

  def copyFields(src: R, dst: R) {
    fields.foreach { f =>
        val value = getField(src, f.asInstanceOf[Field[Any, R]]).value
        getField(dst, f.asInstanceOf[Field[Any, R]]).set(value)
    }
  }

  def getField[T](record: R, field: Field[T, R]): Field[T, R] =
    methodsMap(field).invoke(record) match {
      case a: Association[T, R, _] => a.field
      case f: Field[T, R] => f
      case _ => throw new ORMException("Could not retrieve a field.")
    }

  /*! You can declare explicitly that certain associations should always be prefetched
  whenever a relation participates in a `Criteria` query. To do so simply call the
  `prefetch` method inside relation initialization code. Note that the order of
  association prefetching is important; for more information refer to `Criteria`
  documentation.
  */
  protected var _prefetchSeq: Seq[Association[_, _, _]] = Nil
  def prefetchSeq = _prefetchSeq
  def prefetch[K, C <: Record[_, C], P <: Record[K, P]](
        association: Association[K, C, P]): this.type = {
    this._prefetchSeq ++= List(association)
    this
  }

  /*!## Constraints & Indexes Definition

  Circumflex ORM allows you to define constraints and indexes inside the
  relation body using DSL style.
  */
  def CONSTRAINT(name: String) = new ConstraintHelper(name, this)
  def UNIQUE(columns: ValueHolder[_, R]*) =
    CONSTRAINT(relationName + "_" + columns.map(_.name).mkString("_") + "_key")
        .UNIQUE(columns: _*)

  /*!## Auxiliary Objects

  Auxiliary database objects like triggers, sequences and stored procedures
  can be attached to relation using `addPreAux` and `addPostAux` methods:
  the former one indicates that the auxiliary object will be created before
  the creating of all the tables, the latter indicates that the auxiliary
  object creation will be delayed until all tables are created.
  */
  protected var _preAux: List[SchemaObject] = Nil
  def preAux: Seq[SchemaObject] = _preAux
  def addPreAux(objects: SchemaObject*): this.type = {
    objects.foreach(o => if (!_preAux.contains(o)) _preAux ++= List(o))
    this
  }

  protected var _postAux: List[SchemaObject] = Nil
  def postAux: Seq[SchemaObject] = _postAux
  def addPostAux(objects: SchemaObject*): this.type = {
    objects.foreach(o => if (!_postAux.contains(o)) _postAux ++= List(o))
    this
  }

  /*!## Events

  Relation allows you to attach listeners to certain lifecycle events of its records.
  Following events are available:

    * `beforeInsert`
    * `afterInsert`
    * `beforeUpdate`
    * `afterUpdate`
    * `beforeDelete`
    * `afterDelete`
  */
  protected var _beforeInsert: Seq[R => Unit] = Nil
  def beforeInsert = _beforeInsert
  def beforeInsert(callback: R => Unit): this.type = {
    this._beforeInsert ++= List(callback)
    this
  }
  protected var _afterInsert: Seq[R => Unit] = Nil
  def afterInsert = _afterInsert
  def afterInsert(callback: R => Unit): this.type = {
    this._afterInsert ++= List(callback)
    this
  }
  protected var _beforeUpdate: Seq[R => Unit] = Nil
  def beforeUpdate = _beforeUpdate
  def beforeUpdate(callback: R => Unit): this.type = {
    this._beforeUpdate ++= List(callback)
    this
  }
  protected var _afterUpdate: Seq[R => Unit] = Nil
  def afterUpdate = _afterUpdate
  def afterUpdate(callback: R => Unit): this.type = {
    this._afterUpdate ++= List(callback)
    this
  }
  protected var _beforeDelete: Seq[R => Unit] = Nil
  def beforeDelete = _beforeDelete
  def beforeDelete(callback: R => Unit): this.type = {
    this._beforeDelete ++= List(callback)
    this
  }
  protected var _afterDelete: Seq[R => Unit] = Nil
  def afterDelete = _afterDelete
  def afterDelete(callback: R => Unit): this.type = {
    this._afterDelete ++= List(callback)
    this
  }

  /*!## Equality & Others

  Two relations are considered equal if they share the record class and the same name.

  The `hashCode` method delegates to record class.

  The `canEqual` method indicates that two relations share the same record class.

  Record-specific methods derived from `Record` throw an exception when invoked against relation.
  */
  override def equals(that: Any) = that match {
    case that: Relation[_, _] =>
      this.recordClass == that.recordClass &&
          this.relationName == that.relationName
    case _ => false
  }

  override def hashCode = this.recordClass.hashCode

  override def canEqual(that: Any): Boolean = that match {
    case that: Relation[_, _] =>
      this.recordClass == that.recordClass
    case that: Record[_, _] =>
      this.recordClass == that.getClass
    case _ => false
  }

  override def refresh(): Nothing =
    throw new ORMException("This method cannot be invoked on relation instance.")
  override def validate(): Nothing =
    throw new ORMException("This method cannot be invoked on relation instance.")
  override def INSERT_!(fields: Field[_, R]*): Nothing =
    throw new ORMException("This method cannot be invoked on relation instance.")
  override def UPDATE_!(fields: Field[_, R]*): Nothing =
    throw new ORMException("This method cannot be invoked on relation instance.")
  override def DELETE_!(): Nothing =
    throw new ORMException("This method cannot be invoked on relation instance.")
}

/*!## Implicit Conversions

`Relation` is converted to `RelationNode` implicitly if necessary. If this happens,
the default alias `this` will be assigned to the node. Use `AS` method perform the
explicit conversion if you need to specify an alias manually.
*/
object Relation {
  implicit def toNode[PK, R <: Record[PK, R]](relation: Relation[PK, R]): RelationNode[PK, R] =
    new RelationNode[PK, R](relation)
}

/*!# Table {#table}

The `Table` class represents plain-old database table which will be created to store
records.
*/
trait Table[PK, R <: Record[PK, R]] extends Relation[PK, R] { this: R =>
  def isReadOnly: Boolean = false
  def objectName: String = "TABLE " + qualifiedName
  def sqlCreate: String = {
    init()
    ormConf.dialect.createTable(this)
  }
  def sqlDrop: String = {
    init()
    ormConf.dialect.dropTable(this)
  }
}

/*!# View {#view}

The `View` class represents a database view, whose definition is designated by
the `query` method. By default we assume that views are not updateable, so
DML operations are not allowed on view records. If you implement updateable
views on backend somehow (with triggers in Oracle or rules in PostgreSQL),
override the `isReadOnly` method accordingly.
*/
trait View[PK, R <: Record[PK, R]] extends Relation[PK, R] { this: R =>
  def isReadOnly: Boolean = true
  def objectName: String = "VIEW " + qualifiedName
  def sqlDrop: String = {
    init()
    ormConf.dialect.dropView(this)
  }
  def sqlCreate: String = {
    init()
    ormConf.dialect.createView(this)
  }
  def query: Select[_]
}