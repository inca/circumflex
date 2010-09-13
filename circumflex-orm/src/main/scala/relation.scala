package ru.circumflex.orm

import ru.circumflex.core._

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

  /*! If the relation follows default conventions of Circumflex ORM (about
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

  /*! The `readOnly_?()` method is used to indicate whether the DML operations
  are allowed with this relation. Tables usually allow them and views don't.
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
