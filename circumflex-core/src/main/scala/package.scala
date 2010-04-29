package ru.circumflex

import ru.circumflex.core._
import util.matching.Regex
import org.slf4j.LoggerFactory


package object core {
  /*
   * Converters
   */

  implicit def regexToRichRegex(regex: Regex): RichRegex = new RichRegex(regex)
  implicit def symbolToString(sym: Symbol): String = sym.name
  
  implicit def stringToStringMatcher(str: String): StringMatcher = new SimpleMatcher(str)
  implicit def regexToStringMatcher(regex: Regex): StringMatcher = new RegexMatcher(regex)

  @inline implicit def anyToOption[A](a: A): Option[A] = if (a == null) None else Some(a)

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