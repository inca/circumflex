package ru.circumflex

import ru.circumflex.core._
import util.matching.Regex


package object core {
  implicit def regex2RichRegex(regex: Regex): RichRegex = new RichRegex(regex)
  implicit def symbol2String(sym: Symbol): String = sym.name
  
  implicit def string2StringMatcher(str: String): StringMatcher = new SimplifiedMatcher(str)
  implicit def regex2StringMatcher(regex: Regex): StringMatcher = new RegexMatcher(regex)

  def ctx = Circumflex.ctx
}