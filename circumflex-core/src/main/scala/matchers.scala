package ru.circumflex.core

import java.net.URLDecoder
import javax.servlet.http.HttpServletRequest
import util.matching.Regex
import collection.mutable.ListBuffer
import RichRegex._

trait RequestMatcher {

  def apply(request: HttpServletRequest): Option[Map[String, RequestParams]]

}

case class UriMatcher(val path: String) extends RequestMatcher {

  val keys = new ListBuffer[String]()
  val uriRegex = (""":(\w+)|[\*.+()]""".r.replaceAllInF(path) {
    case sym@("*" | "+") =>
      keys += "splat"
      "(." + sym + "?)"

    case sym@("." | "(" | ")") =>
      "\\" + sym

    case name =>
      keys += name.substring(1)
      "([^/?&#]+)"
  }).r

  def apply(request: HttpServletRequest) = {
    val uri = URLDecoder.decode(request.getRequestURI, "UTF-8")
    uriRegex.allMatches(uri, i => keys(i - 1)) map {
      params => Map("uri" -> new RequestParams(uri, params))
    }
  }
}

case class UriRegexMatcher(val uriRegex: Regex) extends RequestMatcher {

  def apply(request: HttpServletRequest) = {
    val uri = URLDecoder.decode(request.getRequestURI, "UTF-8")
    uriRegex.allMatches(uri, _ => "splat") map {
      params => Map("uri" -> new RequestParams(uri, params))
    }
  }

}

case class HeadersRegexMatcher(val criteria: (String, String)*) extends RequestMatcher {

  def apply(request: HttpServletRequest): Option[Map[String, RequestParams]] = {
    var params = Map[String, RequestParams]()
    for ((headerName, pattern) <- criteria.toList) {
      val headerValue = request.getHeader(headerName)
      pattern.r.allMatches(headerValue, _ => "splat") match {
        case None         => return None
        case Some(ps) => params += headerName -> new RequestParams(headerValue, ps)
      }
    }
    Some(params)
  }

}