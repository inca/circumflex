package ru.circumflex

import ru.circumflex.core._
import util.matching.Regex
import org.slf4j.LoggerFactory


package object core {
  /*
   * Converters
   */

  implicit def symbol2string(sym: Symbol): String = sym.name
  
  @inline implicit def any2option[A](a: A): Option[A] = if (a == null) None else Some(a)

  implicit def string2paramHelper(str: String): ParamHelper = new ParamHelper(str)
  implicit def symbol2paramHelper(sym: Symbol): ParamHelper = new ParamHelper(sym.name)

  /*
   * Types useful
   */

  type MutableMap[A, B] = collection.mutable.Map[A, B]
  val MutableMap  = collection.mutable.Map
  
  /*
   * Circumflex
   */

  val cxLog = LoggerFactory.getLogger("ru.circumflex.core")

  def context = CircumflexContext.context

}