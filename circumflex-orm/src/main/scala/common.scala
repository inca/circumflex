package ru.circumflex.orm

import ORM._

/*!# Common interfaces

Circumflex ORM employs some basic constructs and interfaces, which are used
throughout the framework:

  * `SQLable` represents objects capable of rendering themselves into SQL
  statements;
  * `ParameterizedExpression` represents expression with JDBC-style parameters;
  * `SchemaObject` represents a database schema objects capable of rendering
  `sqlCreate` and `sqlDrop` DDL statements;
  * `ValueHolder` is an extensible atomic data carrier unit of record.

*/
trait SQLable {
  def toSql: String
}

trait ParameterizedExpression extends SQLable {

  /**
   * The parameters associated with this expression. The order is important.
   */
  def parameters: Seq[Any]

  /**
   * Renders this query by replacing parameter placeholders with actual values.
   */
  def toInlineSql: String = parameters.foldLeft(toSql)((sql, p) =>
    sql.replaceFirst("\\?", typeConverter.escape(p)))

  override def equals(that: Any) = that match {
    case e: ParameterizedExpression =>
      e.toSql == this.toSql && (e.parameters.toList -- this.parameters.toList) == Nil
   case _ => false
  }

  override def hashCode = 0

  override def toString = toSql
}

trait SchemaObject {
  /**
   * SQL statement to create this database object.
   */
  def sqlCreate: String

  /**
   * SQL statement to drop this database object.
   */
  def sqlDrop: String

  /**
   * SQL object name. It is used to uniquely identify this object
   * during schema creation by `DDL` to avoid duplicates and to print
   * nice messages on schema generation.
   *
   * We follow default convention to name objects:
   *
   *     <TYPE OF OBJECT> <qualified_name>
   *
   * where `TYPE OF OBJECT` is `TABLE`, `VIEW`, `SEQUENCE`, `TRIGGER`,
   * `FUNCTION`, `PROCEDURE`, `INDEX`, etc. (note the upper case), and
   * `qualified_name` is object's unique identifier.
   *
   * For equality testing, object names are taken in case-insensitive manner
   * (e.g. `MY_TABLE` and `my_table` are considered equal).
   */
  def objectName: String

  override def hashCode = objectName.toLowerCase.hashCode

  override def equals(obj: Any) = obj match {
    case so: SchemaObject => so.objectName.equalsIgnoreCase(this.objectName)
    case _ => false
  }

  override def toString = objectName
}

abstract class ValueHolder[T](val name: String, val record: Record[_]) {

  val uuid = record.uuid + "." + name

  // An internally stored value
  protected var _value: T = _

  protected var _setter: T => T = t => t

  // This way the value will be unwrapped by FTL engine
  def item = getValue

  // Should the `NOT NULL` constraint be applied to this value holder?
  protected var _notNull: Boolean = true
  def nullable_?(): Boolean = !_notNull

  def notNull: this.type = {
    _notNull = true
    return this
  }
  def NOT_NULL: this.type = notNull

  def nullable: this.type = {
    _notNull = false
    return this
  }
  def NULLABLE: this.type = nullable

  def setter = _setter
  def setter(sf: T => T): this.type = {
    _setter = sf
    return this
  }
  def SETTER(sf: T => T): this.type = setter(sf)

  // Getters
  def getValue(): T = _value
  def apply(): T = getValue
  def getOrElse(default: T) = get().getOrElse(default)
  def get(): Option[T] = getValue match {
    case null => None
    case value => Some(value)
  }

  def empty_?(): Boolean = getValue() == null
  def null_?(): Boolean = empty_?
  def NULL_?(): Boolean = empty_?

  // Setters
  def setValue(newValue: T): this.type = {
    _value = if (newValue != null) _setter(newValue) else newValue
    return this
  }
  def :=(newValue: T): this.type = setValue(newValue)
  def update(newValue: T): this.type = setValue(newValue)

  def setNull(): this.type = setValue(null.asInstanceOf[T])
  def null_!() = setNull()
  def NULL_!() = null_!()

  // Equality
  override def equals(that: Any) = that match {
    case vh: ValueHolder[T] => vh.uuid == this.uuid
    case _ => false
  }
  override def hashCode = this.uuid.hashCode

  /**
   * Return a `String` representation of internal value.
   */
  def asString(default: String = "") = if (getValue == null) default else getValue.toString

  /**
   * Return `uuid` as this holder's identifier.
   */
  override def toString = uuid
}