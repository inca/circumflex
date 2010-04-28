package ru.circumflex

import ru.circumflex.core._
import util.matching.Regex
import org.slf4j.LoggerFactory


package object core {
  /*
   * Converters
   */

  implicit def regex2RichRegex(regex: Regex): RichRegex = new RichRegex(regex)
  implicit def symbol2String(sym: Symbol): String = sym.name
  
  implicit def string2StringMatcher(str: String): StringMatcher = new SimpleMatcher(str)
  implicit def regex2StringMatcher(regex: Regex): StringMatcher = new RegexMatcher(regex)

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