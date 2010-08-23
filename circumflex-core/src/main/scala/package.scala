package ru.circumflex

import org.slf4j.LoggerFactory

/*!# The `core` Package

Package `core` contains different utilities and implicits.
They do not relate to public API, but you should import the whole
package if you intend to use Circumflex API:

    import ru.circumflex.core._
*/

package object core {

  /*! Logger is accessed throughout Circumflex. */

  val CX_LOG = LoggerFactory.getLogger("ru.circumflex.core")

  /*! Shortcuts let you access Circumflex singletons and helpers conveniently,
  in a DSL-like style. */

  val cx = Circumflex
  def ctx = Context.get
  def msg = cx.get[MessageResolver]("cx.messages", DefaultMessageResolver)

  /*! Next come some neat utilities and useful implicits. */

  def camelCaseToUnderscore(arg: String) = arg.replaceAll("(?<!^)([A-Z])","_$1").toLowerCase

  def time(block: => Unit): Long = {
    val startTime = System.currentTimeMillis
    block
    System.currentTimeMillis - startTime
  }

  @inline implicit def any2option[T](obj: T): Option[T] = if (obj == null) None else Some(obj)
  @inline implicit def symbol2string(sym: Symbol): String = sym.name

}
