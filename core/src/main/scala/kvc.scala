package circumflex
package core

import java.text.SimpleDateFormat
import java.util.Date
import java.io.Serializable

/*!# Key-value coercion

Trait `KeyValueCoercion` mixed into any key-value generic storage
(like `Map[String, Any]`) brings a bunch of methods for coercing
`Any`-typed values to common types.

The methods are:

  * `getX(key): Option[X]`, where `X` is one of following types:
    `String`, `Int`, `Long`, `Short`, `Byte`, `Double`, `Float`,
    `BigDecimal`, `Boolean`;

  * `getDate(key, pattern): Option[Date]` attempts to parse a date with
    `SimpleDateFormat` and specified `pattern`;

  * `getTimestamp(key): Option[Date]` attempts to parse a timestamp with
    RFC 822 time zone (using pattern `yyyy-MM-dd HH:mm:ss Z`).

  * `instantiate[C](key, default: C)` creates an instance of the class
    specified by the retrieved value, or return specified `default` (by-name).
    The class _must_ have a zero-argument constructor.

*/

trait KeyValueCoercion extends Serializable {

  def get(key: String): Option[_]

  def getAs[C](key: String): Option[C] = try {
    get(key).asInstanceOf[Option[C]]
  } catch {
    case e: ClassCastException =>
      None
  }

  def safeGet[T](key: String)(convert: Any => T): Option[T] =
    get(key).flatMap { v =>
      try {
        Some(convert(v))
      } catch {
        case e: Exception => None
      }
    }

  def getString(key: String): Option[String] =
    get(key).map(_.toString)

  def getInt(key: String): Option[Int] =
    safeGet(key) { _.toString.toInt }

  def getLong(key: String): Option[Long] =
    safeGet(key) { _.toString.toLong }

  def getShort(key: String): Option[Short] =
    safeGet(key) { _.toString.toShort }

  def getByte(key: String): Option[Byte] =
    safeGet(key) { _.toString.toByte }

  def getDouble(key: String): Option[Double] =
    safeGet(key) { _.toString.toDouble }

  def getFloat(key: String): Option[Float] =
    safeGet(key) { _.toString.toFloat }

  def getBigDecimal(key: String): Option[BigDecimal] =
    safeGet(key) { v => BigDecimal(v.toString) }

  def getBoolean(key: String): Option[Boolean] =
    safeGet(key) { _.toString.toBoolean }

  def getDate(key: String, pattern: String = "yyyy-MM-dd"): Option[Date] =
    safeGet(key) { v =>
      val fmt = new SimpleDateFormat(pattern)
      fmt.parse(v.toString)
    }

  def getTimestamp(key: String): Option[Date] =
    getDate(key, "yyyy-MM-dd HH:mm:ss Z")

  /*!## Instantiation facility

The `instantiate` method attempts to create an instance of class, which
is resolved from the storage by specified `key`. In other words, if your storage
contains following key-value record:

```
myKey=com.myapp.MyClass
```

then calling `storage.instantiate("myKey")` will return new instance of
class `com.myapp.MyClass`.

This instantiation facility is used in Circumflex components
as a dead-simple dependency injection mechanism.

### Singletons

Instantiation works great with Scala singletons (objects) as long
as fully qualified class name includes the dollar sign `$` at the end:

``` {.scala}
package com.myapp

object MySingleton // class name is com.myapp.MySingleton$
```
*/

  def instantiate[C](name: String): C = instantiate(name,
    throw new CircumflexException("Could not instantiate '" + name + "'. " +
        "Please make sure that configuration parameter is set to " +
        "fully-qualified name of implementing class."))

  def instantiate[C](name: String, default: => C): C = get(name) match {
    case Some(c: Class[_]) => _instantiate[C](name, c)
    case Some(s: String) if (s.trim() != "") =>
      _instantiate[C](name, Class.forName(s.trim()))
    case v => default
  }

  protected def _instantiate[C](name: String, c: Class[_]): C = try {
    c.getField("MODULE$").get(null).asInstanceOf[C]
  } catch {
    case e: Exception => c.newInstance.asInstanceOf[C]
  }

}
