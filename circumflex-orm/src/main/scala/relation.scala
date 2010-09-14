package ru.circumflex.orm

import ru.circumflex.core._
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
trait Relation[PK, R <: Record[PK, R]] extends Record[PK, R] with SchemaObject { this: R =>

  /*!## Metadata

  Relation metadata contains operational information about it's records by
  introspecting current instance upon initialization.
  */
  protected[orm] var _methodsMap: Map[Field[_], Method] = Map()
  def methodsMap: Map[Field[_], Method] = _methodsMap

  protected[orm] var _fields: List[Field[_]] = Nil
  def fields: Seq[Field[_]] = _fields

  protected[orm] var _associations: List[Association[_, R, _]] = Nil
  def associations: Seq[Association[_, R, _]] = _associations

  protected[orm] var _constraints: List[Constraint] = Nil
  def constraints: Seq[Constraint] = _constraints

  /*!## Auxiliary Objects

  Auxiliary database objects like triggers, sequences and stored procedures
  can be attached to relation using `addPreAux` and `addPostAux` methods:
  the former one indicates that the auxiliary object will be created before
  the creating of all the tables, the latter indicates that the auxiliary
  object creation will be delayed until all tables are created.
  */
  protected[orm] var _preAux: List[SchemaObject] = Nil
  def preAux: Seq[SchemaObject] = _preAux
  def addPreAux(objects: SchemaObject*): this.type = {
    objects.foreach(o => if (!_preAux.contains(o)) _preAux ++= List(o))
    return this
  }

  protected[orm] var _postAux: List[SchemaObject] = Nil
  def postAux: Seq[SchemaObject] = _postAux
  def addPostAux(objects: SchemaObject*): this.type = {
    objects.foreach(o => if (!_postAux.contains(o)) _postAux ++= List(o))
    return this
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
    return this
  }
  protected var _afterInsert: Seq[R => Unit] = Nil
  def afterInsert = _afterInsert
  def afterInsert(callback: R => Unit): this.type = {
    this._afterInsert ++= List(callback)
    return this
  }
  protected var _beforeUpdate: Seq[R => Unit] = Nil
  def beforeUpdate = _beforeUpdate
  def beforeUpdate(callback: R => Unit): this.type = {
    this._beforeUpdate ++= List(callback)
    return this
  }
  protected var _afterUpdate: Seq[R => Unit] = Nil
  def afterUpdate = _afterUpdate
  def afterUpdate(callback: R => Unit): this.type = {
    this._afterUpdate ++= List(callback)
    return this
  }
  protected var _beforeDelete: Seq[R => Unit] = Nil
  def beforeDelete = _beforeDelete
  def beforeDelete(callback: R => Unit): this.type = {
    this._beforeDelete ++= List(callback)
    return this
  }
  protected var _afterDelete: Seq[R => Unit] = Nil
  def afterDelete = _afterDelete
  def afterDelete(callback: R => Unit): this.type = {
    this._afterDelete ++= List(callback)
    return this
  }
  

  /*!## Commons

  If the relation follows default conventions of Circumflex ORM (about
  companion objects), then record class is inferred automatically. Otherwise
  you should override the `recordClass` method.
   */
  val _recordClass: Class[R] = Class.forName(this.getClass.getName.replaceAll("\\$(?=\\Z)", ""))
      .asInstanceOf[Class[R]]
  def recordClass: Class[R] = _recordClass

  /*! By default the relation name is inferred from `recordClass` by replacing
  camelcase delimiters with underscores (for example, record with class
  `ShoppingCartItem` will have a relation with name `shopping_cart_item`).
  You can override `relationName` to use different name.
  */
  val _relationName = camelCaseToUnderscore(recordClass.getSimpleName)
  def relationName = _relationName

  /*! Default schema name is configured via the `orm.defaultSchema` configuration property.
  You may provide different schema for different relations by overriding their `schema` method.
   */
  def schema: Schema = defaultSchema

  /*! The `readOnly_?()` method is used to indicate whether the DML operations
  are allowed with this relation. Tables usually allow them and views usually don't.
   */
  def readOnly_?(): Boolean

  /*!## Equality & Others

  Two relations are considered equal if they share the record class and the same name.

  The `hashCode` method delegates to record class.

  The `canEqual` method indicates that two relations share the same record class.
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
    case _ => false
  }

}

/*!# Table {#table}

The `Table` class represents plain-old database table which will be created to store
records.
*/
trait Table[PK, R <: Record[PK, R]] extends Relation[PK, R] { this: R =>
  def readOnly_?(): Boolean = false
  def sqlCreate: String = "todo"
  def sqlDrop: String = "todo"
  def objectName: String = "TABLE " + relationName
}
