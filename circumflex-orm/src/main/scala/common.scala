package ru.circumflex.orm

import ru.circumflex.core._

/*!# SQLable

Every object capable of rendering itself into an SQL statement
should extend the `SQLable` trait.
*/
trait SQLable {
  def toSql: String
}

/*!# Parameterized expressions

The `ParameterizedExpression` trait provides basic functionality for dealing
with SQL expressions with JDBC-style parameters.
*/
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
      e.toSql == this.toSql && e.parameters.toList == this.parameters.toList
    case _ => false
  }

  override def hashCode = 0

  override def toString = toSql
}

/*!# Schema Object

Every database object which could be created or dropped should
implement the `SchemaObject` trait.
*/
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

/*!# Value holders

Value holder is an atomic data-carrier unit of a record. Two implementations
of `ValueHolder` are known: `Field` and `Association`.
*/
abstract class ValueHolder[T, R <: Record[_, R]](
    val name: String, val record: R, val sqlType: String)
    extends Equals with Wrapper[Option[T]] {

  def item = value

  /*!## Setters

  Setters provide a handy mechanism for preprocessing values before
  setting them. They are functions `T => T` which are applied one-by-one
  each time you set new non-null value. You can add a setter by invoking
  the `addSetter` method:

      val pkg = "package".TEXT.NOT_NULL
          .addSetter(_.trim)
          .addSetter(_.toLowerCase)
          .addSetter(_.replaceAll("/","."))

      pkg := "  ru/circumflex/ORM  "  // "ru.circumflex.orm" will be assigned

  */
  protected var _setters: Seq[T => T] = Nil
  def setters: Seq[T => T] = _setters
  def addSetter(f: T => T): this.type = {
    _setters ++= List(f)
    return this
  }

  /*!## Accessing & Setting Values

  Values are stored internally as `Option[T]`. `None` stands both for
  uninitialized and `null` values. Following examples show how field values
  can be accessed or set:

      val id = "id" BIGINT

      // accessing
      id.value    // Option[Long]
      id.get      // Option[Long]
      id()        // Long or exception
      getOrElse(default: Long)  // Long

      // setting
      id.set(Some(1l))
      id.setNull
      id := 1l

  The `null_?` method indicates whether the underlying value is `null` or not.
  */
  protected var _value: Option[T] = None

  // Accessing

  def value: Option[T] = _value
  def get: Option[T] = value
  def apply(): T = value.get
  def getOrElse(default: T): T = value.getOrElse(default)

  def null_?(): Boolean = value == None

  // Setting

  def set(v: Option[T]): this.type = {
    // disallow setting values on relation fields
    if (record.isInstanceOf[Relation[_, _]])
      throw new ORMException("Could not set the value of the field which belong to relation.")
    // process value with setters
    _value = v.map { v =>
      setters.foldLeft(v) { (v, f) => f(v) }
    }
    return this
  }
  def setNull: this.type = set(None)
  def :=(v: T): Unit = set(Some(v))

  /*!## Column Definition Methods

  Following methods help you construct a definition of the column where
  the field will be persisted:

    * `NOT_NULL` will render `NOT NULL` constraint in column's definition;
    note that starting from 2.0, by default the `NOT NULL` constraint is
    omitted and `NULLABLE` construct is no longer supported;
    * `DEFAULT` will render the `DEFAULT` expression in column's definition
    (if not overriden by dialect);
    * `UNIQUE` will create a `UNIQUE` constraint for enclosing table on
    the field.
  */
  protected var _notNull: Boolean = false
  def notNull_?(): Boolean = _notNull
  def NOT_NULL(): this.type = {
    _notNull = true
    return this
  }

  protected var _unique: Boolean = false
  def unique_?(): Boolean = _unique
  def UNIQUE(): this.type = {
    _unique = true
    return this
  }

  protected var _defaultExpression: Option[String] = None
  def defaultExpression: Option[String] = _defaultExpression
  def DEFAULT(expr: String): this.type = {
    _defaultExpression = Some(expr)
    return this
  }

  /*!## Methods from `Option`

  Since `ValueHolder` is just a wrapper around `Option`, we provide
  some methods to work with your values in functional style
  (they delegate to their equivalents in `Option`).
  */
  def map[B](f: T => B): Option[B] =
    value.map(f)
  def flatMap[B](f: T => Option[B]): Option[B] =
    value.flatMap(f)
  def orElse[B >: T](alternative: => Option[B]): Option[B] =
    value.orElse(alternative)

  /*!## Equality & Others

  Two fields are considered equal if they belong to the same type of records
  and share the same name.

  The `hashCode` calculation is delegated to underlying `value`.

  The `canEqual` method indicates whether the two fields belong to the same
  type of records.

  Finally, `toString` returns the qualified name of relation which it
  belongs to followed by a dot and the field name.
  */
  override def equals(that: Any): Boolean = that match {
    case that: ValueHolder[_, _] => this.canEqual(that) &&
      this.name == that.name
    case _ => false
  }
  override def hashCode: Int = record.hashCode * 31 + name.hashCode
  def canEqual(that: Any): Boolean = that match {
    case that: ValueHolder[_, _] => this.record.canEqual(that.record)
    case _ => false
  }
  override def toString: String = record.relation.qualifiedName + "." + name
}