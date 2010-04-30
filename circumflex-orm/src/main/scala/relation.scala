package ru.circumflex.orm

import ORM._
import JDBC._
import ru.circumflex.core.Circumflex
import ru.circumflex.core.CircumflexUtil._
import java.lang.reflect.Method
import java.sql.PreparedStatement

// ## Relations registry

/**
 * This singleton holds mappings between `Record` classes and their
 * corresponding relations. This provides weak coupling between the two
 * to allow proper initialization of either side.
 */
object RelationRegistry {

  protected var classToRelation: Map[Class[_], Relation[_]] = Map()

  def getRelation[R <: Record[R]](r: R): Relation[R] =
    classToRelation.get(r.getClass) match {
      case Some(rel: Relation[R]) => rel
      case _ => {
        val relClass = Circumflex.loadClass[Relation[R]](r.getClass.getName + "$")
        val relation = relClass.getField("MODULE$").get(null).asInstanceOf[Relation[R]]
        classToRelation += (r.getClass -> relation)
        relation
      }
    }

}

// ## Relation

abstract class Relation[R <: Record[R]] {

  // ### Commons

  protected[orm] var _fields: List[Field[_]] = Nil
  def fields: Seq[Field[_]] = _fields

  protected[orm] var _associations: List[Association[R, _]] = Nil
  def associations: Seq[Association[R, _]] = _associations

  protected[orm] var _constraints: List[Constraint] = Nil
  def constraints: Seq[Constraint] = _constraints

  protected[orm] var _preAux: List[SchemaObject] = Nil
  def preAux: Seq[SchemaObject] = _preAux

  protected[orm] var _postAux: List[SchemaObject] = Nil
  def postAux: Seq[SchemaObject] = _postAux

  // Since we use reflection to read `Record`s from database
  // we need this map to remember, what `Method` on `Record`
  // instance should we invoke for each `Field`.
  protected[orm] var _methodsMap: Map[Field[_], Method] = Map()
  def methodsMap: Map[Field[_], Method] = _methodsMap

  /**
   * Attempt to find a record class by convention of companion object,
   * e.g. strip trailing `$` from `this.getClass.getName`.
   */
  val recordClass: Class[R] = Circumflex
      .loadClass[R](this.getClass.getName.replaceAll("\\$(?=\\Z)", ""))

  /**
   * This sample is used to introspect record for fields, constraints and,
   * possibly, other stuff.
   */
  protected[orm] val recordSample: R = recordClass.newInstance
  def r: R = recordSample
  def >() = r

  /**
   * Unique identifier based on `recordClass` to identify this relation
   * among others.
   */
  val uuid = recordSample.uuid

  /**
   * Relation name defaults to record's unqualified class name, transformed
   * with `Circumflex.camelCaseToUnderscore`.
   */
  val relationName = camelCaseToUnderscore(recordClass.getSimpleName)

  /**
   * Schema is used to produce a qualified name for relation.
   */
  val schema: Schema = defaultSchema

  /**
   * Obtain a qualified name for this relation from dialect.
   */
  val qualifiedName = dialect.relationQualifiedName(this)

  /**
   * Are DML statements allowed against this relation?
   */
  def readOnly_?(): Boolean = false

  /**
   * Primary key field of this relation.
   */
  def primaryKey = recordSample.id

  /**
   * Create new `RelationNode` with specified `alias`.
   */
  def as(alias: String) = new RelationNode[R](this).as(alias)

  /**
   * Try to find an association to specified `relation`.
   */
  def findAssociation[F <: Record[F]](relation: Relation[F]): Option[Association[R, F]] =
    associations.find(_.foreignRelation == relation)
        .asInstanceOf[Option[Association[R, F]]]

  // ### Introspection and Initialization

  /**
   * Inspect `recordClass` to find fields and constraints definitions.
   */
  private def findMembers(cl: Class[_]): Unit = {
    if (cl != classOf[Any]) findMembers(cl.getSuperclass)
    cl.getDeclaredFields
        .map(f => cl.getMethod(f.getName))
        .foreach(m => processMember(m))
  }

  private def processMember(m: Method): Unit = m.getReturnType match {
    case cl if classOf[Field[_]].isAssignableFrom(cl) =>
      val f = m.invoke(recordSample).asInstanceOf[Field[_]]
      this._fields ++= List(f)
      if (f.unique_?) this.unique(f)
      this._methodsMap += f -> m
    case cl if classOf[Association[R, _]].isAssignableFrom(cl) =>
      val a = m.invoke(recordSample).asInstanceOf[Association[R, _]]
      this._associations ++= List[Association[R, _]](a)
      this._fields ++= List(a.field)
      this._methodsMap += a.field -> m
      this._constraints ++= List(associationFK(a))
    case _ =>
  }

  private def associationFK(assoc: Association[_, _]): ForeignKey = {
    val name = relationName + "_" + assoc.name + "_fkey"
    new ForeignKey(this,
      name,
      assoc.foreignRelation,
      List(assoc.field),
      List(assoc.foreignRelation.primaryKey),
      assoc.onDelete,
      assoc.onUpdate)
  }

  findMembers(recordClass)

  /**
   * Allow dialects to override the initialization logic.
   */
  dialect.initializeRelation(this)


  // ### Definitions

  /**
   * A helper for creating named constraints.
   */
  protected[orm] def constraint(name: String): ConstraintHelper =
    new ConstraintHelper(this, name)
  protected[orm] def CONSTRAINT(name: String): ConstraintHelper =
    constraint(name)

  /**
   * Add a unique constraint to this relation's definition.
   */
  protected[orm] def unique(fields: Field[_]*): UniqueKey =
    constraint(relationName + "_" + fields.map(_.name).mkString("_") + "_key")
        .unique(fields: _*)
  protected[orm] def UNIQUE(fields: Field[_]*) = unique(fields: _*)

  /**
   * Add a foreign key constraint to this relation's definition.
   */
  def foreignKey(localFields: Field[_]*): ForeignKeyHelper =
    new ForeignKeyHelper(this, relationName + "_" +
        localFields.map(_.name).mkString("_") + "_fkey", localFields)
  def FOREIGN_KEY(localFields: Field[_]*): ForeignKeyHelper =
    foreignKey(localFields: _*)

  /**
   * Add an index to this relation's definition.
   */
  def index(indexName: String, expressions: String*): Index = {
    val idx = new Index(this, indexName, expressions: _*)
    addPostAux(idx)
    return idx
  }
  def INDEX(indexName: String, expressions: String*): Index =
    index(indexName, expressions: _*)

  /**
   * Add specified `objects` to this relation's `preAux` queue.
   */
  def addPreAux(objects: SchemaObject*): this.type = {
    objects.foreach(o => if (!_preAux.contains(o)) _preAux ++= List(o))
    return this
  }

  /**
   * Add specified `objects` to this relaion's `postAux` queue.
   */
  def addPostAux(objects: SchemaObject*): this.type = {
    objects.foreach(o => if (!_postAux.contains(o)) _postAux ++= List(o))
    return this
  }

  // ### Persistence

  /**
   * A helper to set parameters to `PreparedStatement`.
   */
  protected[orm] def setParams(record: R, st: PreparedStatement, fields: Seq[Field[_]]) =
    (0 until fields.size).foreach(ix => typeConverter.write(st, fields(ix).apply(), ix + 1))

  /**
   * Skips the validation and performs `INSERT` statement for specified `record`
   * using specified `fields`.
   */
  def insert_!(record: R, fields: Seq[Field[_]]): Int = if (readOnly_?)
    throw new ORMException("The relation " + qualifiedName + " is read-only.")
  else transactionManager.dml(conn => {
    val sql = dialect.insertRecord(record, fields)
    sqlLog.debug(sql)
    auto(conn.prepareStatement(sql))(st => {
      setParams(record, st, fields)
      st.executeUpdate
    })
  })
  def INSERT_!(record: R, fields: Seq[Field[_]]) = insert_!(record, fields)

  /**
   * Skips the validation and performs `INSERT` statement for specified `record`.
   * All empty fields with `DEFAULT` expressions are omitted.
   */
  def insert_!(record: R): Int =
    insert_!(record, record.getFields.filter(f => !(f.empty_? && f.default != None)))
  def INSERT_!(record: R) = insert_!(record)

  /**
   * Skips the validation and performs `UPDATE` statement for specified `record`.
   * All empty fields with `DEFAULT` expressions are omitted.
   */
  def update_!(record: R, fields: Seq[Field[_]]): Int = if (readOnly_?)
    throw new ORMException("The relation " + qualifiedName + " is read-only.")
  else transactionManager.dml(conn => {
    val sql = dialect.updateRecord(record, fields)
    sqlLog.debug(sql)
    auto(conn.prepareStatement(sql))(st => {
      setParams(record, st, fields)
      typeConverter.write(st, record.id(), fields.size + 1)
      st.executeUpdate
    })
  })
  def UPDATE_!(record: R, fields: Seq[Field[_]]) = update_!(record, fields)

  /**
   * Skips the validation and performs `UPDATE` statement. All fields are affected
   * except primary key, which is used as an update criteria.
   */
  def update_!(record: R): Int = update_!(record, record.getFields.filter(f => f != primaryKey))
  def UPDATE_!(record: R) = update_!(record)

  /**
   * Executes the `DELETE` statement for specified `record` using it's primary key
   * as delete criteria.
   */
  def delete_!(record: R): Int = if (readOnly_?)
    throw new ORMException("The relation " + qualifiedName + " is read-only.")
  else transactionManager.dml(conn => {
    val sql = dialect.deleteRecord(record)
    sqlLog.debug(sql)
    auto(conn.prepareStatement(sql))(st => {
      typeConverter.write(st, record.id(), 1)
      st.executeUpdate
    })
  })

  // ### Equality and others

  override def equals(that: Any) = that match {
    case r: Relation[R] => r.relationName.equalsIgnoreCase(this.relationName)
    case _ => false
  }

  override def hashCode = this.relationName.toLowerCase.hashCode

  override def toString = qualifiedName

}

// ## Table

abstract class Table[R <: Record[R]] extends Relation[R]
    with SchemaObject {
  val objectName = "TABLE " + qualifiedName
  lazy val sqlDrop = dialect.dropTable(this)
  lazy val sqlCreate = dialect.createTable(this)
}

// ## View

abstract class View[R <: Record[R]] extends Relation[R]
    with SchemaObject {

  /**
   * Views are not updatable by default.
   */
  override def readOnly_?() = true

  /**
   * A `query` that makes up this view definition.
   */
  def query: Select[_]

  // ### Miscellaneous

  val objectName = "VIEW " + qualifiedName
  lazy val sqlDrop = dialect.dropView(this)
  lazy val sqlCreate = dialect.createView(this)
}
