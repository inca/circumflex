package ru.circumflex.orm

import ORM._
import java.lang.String

/*!# Field

The `Field` class holds atomic values of records. It wraps actual value
and provides methods for constructing column definitions for enclosing
tables.

It also provides handy mechanisms for processing values before setting
using the `SETTER` field.
 */
class Field[T](val record: Record[_, _],
               val name: String,
               val sqlType: String) extends Equals {

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
      id.value(Some(1l))
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

  def value(v: Option[T]): this.type = {
    _value = v.map { v =>
      setters.foldLeft(v) { (v, f) => f(v) }
    }
    return this
  }
  def set(v: Option[T]): this.type = value(v)
  def setNull: this.type = value(None)
  def :=(v: T): Unit = value(Some(v))

  /*!## Column Definition Methods

  Following methods help you construct a definition of the column where
  the field will be persisted:

    * `NOT_NULL` will render `NOT NULL` constraint in column's definition;
    note that starting from 2.0, by default the `NOT NULL` constaint is
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

  protected var _defaultExpresion: Option[String] = None
  def defaultExpression: Option[String] = _defaultExpresion
  def DEFAULT(expr: String): this.type = {
    _defaultExpresion = Some(dialect.defaultExpression(expr))
    return this
  }

  /*!## Methods from `Option`

  Since `Field` is just a wrapper around `Option`, we provide
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

  Two fields are considered equal if their values are equal.

  The `hashCode` calculation is delegated to underlying `value`.

  The `canEqual` method indicates whether the two fields belong
  to the same type of records and share the same name.

  Finally, `toString` is delegated to underlying `value`'s
  `toString` method or shows `<undefined>` if the field's value
  is `null`.
  */
  override def equals(that: Any): Boolean = that match {
    case that: Field[T] => this.value == that.value
    case _ => false
  }
  override def hashCode: Int = value.hashCode
  def canEqual(that: Any): Boolean = that match {
    case that: Field[T] => this.record.canEqual(that.record) &&
        this.name == that.name
    case _ => false
  }
  override def toString: String = value.map(_.toString).getOrElse("<undefined>")
}