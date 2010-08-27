package ru.circumflex

import org.slf4j.LoggerFactory

/*!# The `core` Package

Package `core` contains different shortcuts, utilities and implicits.
You should import this package if you intend to use Circumflex API:

    import ru.circumflex.core._
*/
package object core {

  /**
   * Returns a logger instance used by Circumflex core components.
   */
  val CX_LOG = LoggerFactory.getLogger("ru.circumflex.core")

  /**
   * Converts `ThisTypeOfIdentifiers` into `that_type_of_identifiers`.
   */
  def camelCaseToUnderscore(arg: String) = arg.replaceAll("(?<!^)([A-Z])","_$1").toLowerCase


  /**
   * Executes specified `block` and counts it's execution time.
   */
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

  /**
   * A shortcut for accessing the `Circumflex` singleton.
   */
  val cx = Circumflex

  /**
   * A shortcut for accessing current context.
   */
  def ctx = Context.get

  /**
   * A shortcut for accessing global messages resolver.
   */
  def msg = cx.get[MessageResolver]("cx.messages", DefaultMessageResolver)

}
