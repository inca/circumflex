package ru.circumflex.core

import javax.servlet.http.HttpServletRequest
import util.matching.Regex
import collection.mutable.ListBuffer

/* ## Matching result */

class Match(val value: String,
            val prefix: String, // prefix constant of value
            val suffix: String, // all the rest
            params: (String, String)*) {

  def apply(index: Int): String = params(index - 1)._2
  def apply(name: String): String = params.find(_._1 == name).get._2
  def splat: Seq[String] = params.filter(_._1 == "splat").map(_._2).toSeq
  def unapplySeq(ctx: CircumflexContext): Option[Seq[String]] = params.map(_._2).toSeq
  override def toString = value
  
}

/* ## Basics matchers */

trait StringMatcher extends (String => Option[Match])

class RegexMatcher(val regex: Regex = null) extends StringMatcher {

  def groupName(index: Int): String = "splat"

  def apply(value: String) = {
    val m = regex.pattern.matcher(value)
    if (m.matches) {
      val matches = for (i <- 1 to m.groupCount) yield groupName(i) -> m.group(i)
      val prefix = if (m.groupCount > 0) value.substring(0, m.start(1)) else value
      val suffix = if (m.groupCount > 0) value.substring(m.start(1)) else ""
      new Match(value, prefix, suffix, matches: _*)
    } else None
  }

}

class SimpleMatcher(path: String) extends RegexMatcher {

  val keys = ListBuffer[String]()
  
  override val regex = (""":\w+|[\*.+()]""".r.replaceAllInS(path) { s =>
    s match {
      case "*" | "+" =>
        keys += "splat"
        "(." + s + "?)"

      case "." | "(" | ")" =>
        "\\\\" + s

      case _ =>
        keys += s.substring(1)
        "([^/?&#]+)"
    }
  }).r

  override def groupName(index: Int): String = keys(index - 1)

}

/* ## Request matchers */

trait RequestMatcher extends (HttpServletRequest => Option[Map[String, Match]]) {
  private val _matchers = ListBuffer[RequestMatcher]()
  _matchers += this

  /* Composite pattern */
  def &(matcher: RequestMatcher): RequestMatcher = {
    _matchers += matcher
    this
  }

  def apply(request: HttpServletRequest): Option[Map[String, Match]] = {
    var res = Map[String, Match]()
    for (matcher <- _matchers)
      matcher.run(request) match {
        case Some(m) => res += matcher.name -> m
        case None    => return None
      }
    res
  }

  val name: String
  def run(request: HttpServletRequest): Option[Match]
}

class UriMatcher(matcher: StringMatcher) extends RequestMatcher {
  val name = "uri"
  def run(request: HttpServletRequest) = matcher(context.uri)
}

class HeaderMatcher(val name: String, matcher: StringMatcher) extends RequestMatcher {
  def run(request: HttpServletRequest) = matcher(request.getHeader(name))
}