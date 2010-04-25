package ru.circumflex.core

import java.net.URLDecoder
import javax.servlet.http.HttpServletRequest
import util.matching.Regex
import collection.mutable.ListBuffer
import Convertions._

/**
 * Matching result
 */

class Match(val value: String, params: Array[(String, String)]) {

  def apply(index: Int): String = params(index - 1)._2
  def apply(name: String): String = params.find(_._1 == name).get._2
  def splat: Seq[String] = params.filter(_._1 == "splat").map(_._2).toSeq
  def toSeq: Seq[String] = params.map(_._2).toSeq
  override def toString = value
  
}

/**
 * Basics matchers
 */

trait StringMatcher extends (String => Option[Match])

class RegexMatcher(val regex: Regex) extends StringMatcher {

  def this() = this(null)

  def groupName(index: Int): String = "splat"

  def apply(value: String) = {
    val m = regex.pattern.matcher(value)
    if (m.matches) {
      val matches = for (i <- 1 to m.groupCount) yield groupName(i) -> m.group(i)
      Some(new Match(value, Array(matches: _*)))
    } else None
  }

}

class SimplifiedMatcher(path: String) extends RegexMatcher {

  val keys = new ListBuffer[String]()
  
  override val regex = (""":(\w+)|[\*.+()]""".r.replaceAllInF(path) {
    case sym@("*" | "+") =>
      keys += "splat"
      "(." + sym + "?)"

    case sym@("." | "(" | ")") =>
      "\\" + sym

    case name =>
      keys += name.substring(1)
      "([^/?&#]+)"
  }).r

  override def groupName(index: Int): String = keys(index - 1)

}

/**
 * Request matchers
 */

trait RequestMatcher extends (HttpServletRequest => Option[Map[String, Match]])

class UriRequestMatcher(matcher: StringMatcher) extends RequestMatcher {

  def apply(request: HttpServletRequest) =
    matcher(URLDecoder.decode(request.getRequestURI, "UTF-8")) map {
      m => Map("uri" -> m)
    }

}

class HeaderRequestMatcher(criteria: (String, StringMatcher)*) extends RequestMatcher {

  def apply(request: HttpServletRequest): Option[Map[String, Match]] = {
    var params = Map[String, Match]()
    for ((headerName, matcher) <- criteria.toList)
      matcher(request.getHeader(headerName)) match {
        case None    => return None
        case Some(m) => params += headerName -> m
      }
    Some(params)
  }

}