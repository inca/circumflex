package ru.circumflex.core

import java.net.URLDecoder
import javax.servlet.http.HttpServletRequest
import util.matching.Regex
import collection.mutable.ListBuffer
import RichRegex._

trait RequestMatcher {

  def apply(request: HttpServletRequest): Option[Map[String, String]]

}

case class UriMatcher(val path: String) extends RequestMatcher {

  val keys = new ListBuffer[String]()
  val uriRegex = """:(\w+)|[\*.+()]""".r.replaceAllInF(path) {
    case "*" =>
      keys += "uri$" + (keys.length + 1)
      "(.*?)"

    case sym@("." | "+" | "(" | ")") =>
      "\\" + sym

    case name =>
      keys += name.substring(1)
      "([^/?&#]+)"
  }

  def apply(request: HttpServletRequest) =
    uriRegex.r.allMatches(URLDecoder.decode(request.getRequestURI, "UTF-8"), i => keys(i - 1))

}

case class UriRegexMatcher(val uriRegex: String) extends RequestMatcher {

  def apply(request: HttpServletRequest) =
    uriRegex.r.allMatches(URLDecoder.decode(request.getRequestURI, "UTF-8"), "uri$" + _)

}

case class HeadersRegexMatcher(val criteria: (String, String)*) extends RequestMatcher {

  def apply(request: HttpServletRequest): Option[Map[String, String]] = {
    var params = Map[String,String]()
     for ((name, pattern) <- criteria.toList)
      matchHeader(name, request.getHeader(name), pattern.r) match {
        case Some(p) => params ++= p
        case None    => return None
      }
    Some(params)
  }

  def matchHeader(headerName: String, headerValue: String, crit: Regex): Option[Map[String, String]] =
    if (headerValue == null) None
    else crit.allMatches(headerValue, headerName + "$" + _)

}