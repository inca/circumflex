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

  /*! Next come some neat utilities. */

  def camelCaseToUnderscore(arg: String) = arg.replaceAll("(?<!^)([A-Z])","_$1").toLowerCase

  def time(block: => Unit): Long = {
    val startTime = System.currentTimeMillis
    block
    System.currentTimeMillis - startTime
  }

}
