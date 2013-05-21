package circumflex
package core

import java.util.Date
import java.io.Serializable

/*! # Data model support

Circumflex introduces two parameterized wrapper clases
— `Wrapper` and `Container` — to keep certain components
weakly coupled with each other.
*/
trait Wrapper[T] extends Serializable {
  def item: T
}

/*! ## Container

Containers are generic data-carrier units. They wrap mutable variable
with common functionality like setters, accessors, mutators and metadata.
`ValueHolder` of Circumflex ORM uses container functionality, see its
docs for more information.

By convention containers should be tested for equality by their external
attributes (like name, identifier, etc.), **but not their internal value**.
Implementations should provide sensible `canEqual`, `equal` and `hashCode`
methods, but internal value should not be taken into consideration.
*/
trait Container[T] extends Serializable {
  protected var _value: Option[T] = None

  /*! ### Setters

  Setters provide a handy mechanism for preprocessing values before
  setting them. They are functions `T => T` which are applied one-by-one
  each time you set new non-null value.
  */
  protected var _setters: Seq[T => T] = Nil
  def setters: Seq[T => T] = _setters
  def addSetter(f: T => T): this.type = {
    _setters ++= List(f)
    this
  }

  /*! ### Accessing & Setting Values

  Values are stored internally as `Option[T]`. `None` stands both for
  uninitialized and `null` values.
  */
  def value: Option[T] = _value
  def get = value
  def apply(): T = value.get
  def getOrElse(default: => T): T = value.getOrElse(default)
  def isEmpty: Boolean = value == None

  def set(v: Option[T]): this.type = {
    _value = v.map { v =>
      setters.foldLeft(v) { (v, f) => f(v) }
    }
    this
  }
  def set(v: T): this.type = set(any2option(v))
  def setNull(): this.type = set(None)
  def :=(v: T) = set(v)

  /*! ### Methods from `Option`

  Since `ValueHolder` is just a wrapper around `Option`, we provide
  some methods to work with your values in functional style
  (they delegate to their equivalents in `Option`).
  */
  def map[B](f: T => B): Option[B] = value.map(f)

  def flatMap[B](f: T => Option[B]): Option[B] =
    value.flatMap(f)

  def orElse[B >: T](alternative: => Option[B]): Option[B] =
    value.orElse(alternative)

  def filter(predicate: T => Boolean): Option[T] =
    value.filter(predicate)

}

/*! ### Coercion methods

The `Coercion` trait can be mixed into `Container[String]` to add a couple
of common scalar conversion methods (see [[/core/src/main/scala/parse.scala]]).
*/
trait Coercion extends Container[String] {

  def date: Date = dateOption.get

  def dateOption: Option[Date] =
    value.flatMap(v => parse.dateOption(v))

  def date(fmt: String): Date = dateOption(fmt).get

  def dateOption(fmt: String): Option[Date] =
    value.flatMap(v => parse.dateOption(fmt, v))

  def int: Int = intOption.get

  def intOption: Option[Int] =
    value.flatMap(v => parse.intOption(v))

  def long: Long = longOption.get

  def longOption: Option[Long] =
    value.flatMap(v => parse.longOption(v))

  def float: Float = floatOption.get

  def floatOption: Option[Float] =
    value.flatMap(v => parse.floatOption(v))

  def double: Double = doubleOption.get

  def doubleOption: Option[Double] =
    value.flatMap(v => parse.doubleOption(v))

  def boolean: Boolean = booleanOption.get

  def booleanOption: Option[Boolean] =
    value.flatMap(v => parse.booleanOption(v))

  def bigDecimal: BigDecimal = bigDecimalOption.get

  def bigDecimalOption: Option[BigDecimal] =
    value.flatMap(v => parse.bigDecimalOption(v))

}
