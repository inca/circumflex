package ru.circumflex

import org.slf4j.LoggerFactory

/*!# Core Package

The package `core` contains different utilities and implicits.
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

  /*! This thingy is very useful and very light: it translates every
  `ThisKindOfIdentifiers` into `that_kinds_of_identifiers`.
  */
  def camelCaseToUnderscore(arg: String) = arg.replaceAll("(?<!^)([A-Z])","_$1").toLowerCase

  /*! Executes specified `block` and reports the time taken. */
  def time(block: => Unit): Long = {
    val startTime = System.currentTimeMillis
    block
    System.currentTimeMillis - startTime
  }

}