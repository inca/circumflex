package ru.circumflex
package core

/*!# Data model support

To make Circumflex components independent from various view technologies
we introduce some basic interfaces here. Different components implement
these interfaces while view technologies should provide proper support
for them.
*/
trait Wrapper[T] {
  def item: T
}

/*!# Containers

Containers are generic data-carrier units. They wrap mutable variable
with common functionality like setters, accessors, mutators and metadata.
`ValueHolder` of Circumflex ORM uses container functionality, see its
docs for more information.

By convention containers should be tested for equality by their external
attributes (like name, identifier, etc.), **but not their internal value**.
Implementations should provide sensible `canEqual`, `equal` and `hashCode`
methods, but internal value should not be taken into consideration.
*/
trait Container[T] extends Equals {
  protected var _value: Option[T] = None

  /*!## Setters

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

  /*!## Accessing & Setting Values

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

}