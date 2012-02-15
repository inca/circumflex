package pro.circumflex
package core

import java.text.SimpleDateFormat
import java.util.Date

/*!# Key-value coercion

Trait `KeyValueCoercion` mixed into any key-value generic storage
(like `Map[String, Any]`) brings a whole bunch
of methods for quick coercion the Any-typed value to specific type.

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

trait KeyValueCoercion {

  def get(key: String): Option[_]

  def safeGet[T](key: String)(convert: Any => T): Option[T] =
    get(key).flatMap { v =>
      try {
        Some(convert(v))
      } catch {
        case e: Exception => None
      }
    }

  def getString(key: String): Option[String] = get(key).map(_.toString)

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

  def getDate(key: String, pattern: String): Option[Date] =
    safeGet(key) { v =>
      val fmt = new SimpleDateFormat(pattern)
      fmt.parse(v.toString)
    }

  def getTimestamp(key: String): Option[Date] =
    getDate(key, "yyyy-MM-dd HH:mm:ss Z")

  def instantiate[C](name: String, default: => C): C = get(name) match {
    case Some(instance: C) => instance
    case Some(c: Class[C]) => _instantiate[C](name, c)
    case Some(s: String) if (s.trim() != "") =>
      _instantiate[C](name, Class.forName(s.trim()))
    case v => default
  }

  /*!## Dependency Injection

The `instantiate` method is used by Circumflex as dead-simple dependency
injection mechanism.

Consider the following example.

``` {.scala}
// Interface
trait Configurable {
  def doWork()
}

// Default implementation
class DefaultConfigurable {
  def doWork() {
    println("I'm finished!")
  }
}

// Global definition
object conf {
  val configurable = cx.instantiate[Configurable](
      "myapp.configurable", new DefaultConfigurable)
}

// Usage in application
conf.configurable.doWork()
```

Now if you use the above application as a library and need to alter the behavior
of `DefaultConfigurable`, all you have to do is to implement `Configurable` and
specify the class name in `myapp.configurable` configuration parameter.

``` {.scala}
// Custom implementation
class CustomConfigurable extends Configurable {
  def doWork() {
    println("I am way better!")
  }
}

// cx.properties
myapp.configurable=com.myapp.impl.CustomConfigurable
```

This mechanism is used by Circumflex in places, which are designed to be
configurable.
  */
  protected def _instantiate[C](name: String, c: Class[_]): C = try {
    c.getField("MODULE$").get(null).asInstanceOf[C]
  } catch {
    case e: Exception => c.newInstance.asInstanceOf[C]
  }

}
