package ru.circumflex

import ru.circumflex.core._
import util.matching.Regex
import org.slf4j.LoggerFactory


package object core {
  implicit def regex2RichRegex(regex: Regex): RichRegex = new RichRegex(regex)
  implicit def symbol2String(sym: Symbol): String = sym.name
  
  implicit def string2StringMatcher(str: String): StringMatcher = new SimpleMatcher(str)
  implicit def regex2StringMatcher(regex: Regex): StringMatcher = new RegexMatcher(regex)

  val cxLog = LoggerFactory.getLogger("ru.circumflex.core")

  def ctx = Circumflex.ctx
}