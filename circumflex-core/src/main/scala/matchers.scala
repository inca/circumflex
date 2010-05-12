package ru.circumflex.core

import javax.servlet.http.HttpServletRequest
import util.matching.Regex
import collection.mutable.ListBuffer

// ## Matching result

class Match(val value: String,
            params: (String, String)*) {
  def apply(index: Int): String = params(index - 1)._2
  def apply(name: String): String = params.find(_._1 == name).get._2
  def splat: Seq[String] = params.filter(_._1 == "splat").map(_._2).toSeq
  def unapplySeq(ctx: CircumflexContext): Option[Seq[String]] = params.map(_._2).toSeq
  override def toString = value
}

// ## Matchers

trait Matcher {
  def apply(): Option[Seq[Match]]
  def add(matcher: Matcher): CompositeMatcher
  def &(matcher: Matcher) = add(matcher)
}

abstract class AtomicMatcher extends Matcher {
  def add(matcher: Matcher) = new CompositeMatcher().add(matcher)
}

class CompositeMatcher extends Matcher {
  private var _matchers: Seq[Matcher] = Nil
  def matchers = _matchers
  def add(matcher: Matcher): CompositeMatcher = {
    _matchers ++= List(matcher)
    return this
  }
  def apply() = try {
    val matches = _matchers.flatMap(m => m.apply match {
      case Some(matches: Seq[Match]) => matches
      case _ => throw new MatchError
    })
    if (matches.size > 0) Some(matches)
    else None
  } catch {
    case e: MatchError => None
  }
}

/* ## Basics matchers */

trait StringMatcher extends (String => Option[Match])

class RegexMatcher(val regex: Regex = null) extends StringMatcher {

  def groupName(index: Int): String = "splat"

  def apply(value: String) = {
    val m = regex.pattern.matcher(value)
    if (m.matches) {
      val matches = for (i <- 1 to m.groupCount) yield groupName(i) -> m.group(i)
      new Match(value, matches: _*)
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