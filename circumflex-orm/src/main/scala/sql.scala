package ru.circumflex.orm

/*!# Schema objects

Following classes represent various database schema objects:

  * `Schema` corresponds to database schema (or catalog), if such objects
  are supported by database vendor;
  * `Constraint` corresponds to one of database constraint types:

    * `Unique`,
    * `ForeignKey`,
    * `CheckConstraint`;

  * `Index` corresponds to database index.

Circumflex ORM also uses some helpers to make DSL-style data definition.
*/

class Schema(val name: String) extends SchemaObject {
  def objectName = "SCHEMA " + name
  def sqlCreate = "todo"
  def sqlDrop = "todo"
}

abstract class Constraint(val relation: Relation[_, _],
                          val constraintName: String)
    extends SchemaObject with SQLable {

  val objectName = "CONSTRAINT " + constraintName
  val sqlCreate = "todo"
  val sqlDrop = "todo"
  val toSql = "todo"

  def sqlDefinition: String

  override def toString = toSql
}

class UniqueKey(relation: Relation[_, _],
                name: String,
                val fields: Seq[Field[_]])
    extends Constraint(relation, name) {
  def sqlDefinition = "todo"
}

case class ForeignKeyAction(val toSql: String) extends SQLable {
  override def toString = toSql
}

case class JoinType(val toSql: String) extends SQLable {
  override def toString = toSql
}

case class SetOperation(val toSql: String) extends SQLable {
  override def toString = toSql
}

class Order(val expression: String, val parameters: Seq[Any])
    extends ParameterizedExpression {
  protected[orm] var _specificator = dialect.asc
  def ASC: this.type = {
    this._specificator = dialect.asc
    return this
  }
  def DESC: this.type = {
    this._specificator = dialect.desc
    return this
  }
  def toSql = expression + " " + _specificator
}
