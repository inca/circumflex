package ru.circumflex

import org.slf4j.LoggerFactory

/*!# Core Package

The package `core` contains different utilities and implicits.
They do not relate to public API, but you should import the whole
package if you intend to use Circumflex API:

    import ru.circumflex.core._

*/

package object core {

  type MutableMap[A, B] = collection.mutable.Map[A, B]
  val cxLog = LoggerFactory.getLogger("ru.circumflex.core")

}