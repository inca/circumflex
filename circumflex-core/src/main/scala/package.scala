package ru.circumflex

import org.slf4j.LoggerFactory

/*!# The `core` Package

Package `core` contains different shortcuts, utilities and implicits.
You should import this package if you intend to use Circumflex API:

    import ru.circumflex.core._
*/

package object core {

  val CX_LOG = LoggerFactory.getLogger("ru.circumflex.core")

  def camelCaseToUnderscore(arg: String) = arg.replaceAll("(?<!^)([A-Z])","_$1").toLowerCase

  def time(block: => Unit): Long = {
    val startTime = System.currentTimeMillis
    block
    System.currentTimeMillis - startTime
  }

  /*! Implicits let you avoid boilerplates when wrapping nullable objects into `Option` and
  converting `Symbol` instances to `String`s.
   */

  @inline implicit def any2option[T](obj: T): Option[T] = if (obj == null) None else Some(obj)
  @inline implicit def symbol2string(sym: Symbol): String = sym.name

  /*! Shortcuts let you access Circumflex singletons and helpers conveniently,
  in a DSL-like style. */

  val cx = Circumflex
  def ctx = Context.get
  def msg = cx.get[MessageResolver]("cx.messages", DefaultMessageResolver)

}
