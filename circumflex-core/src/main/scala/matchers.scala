package ru.circumflex.core

import java.net.URLDecoder
import javax.servlet.http.HttpServletRequest
import util.matching.Regex

trait RequestMatcher {

  def extractMatches(regex: Regex,
                     src: String,
                     groupPrefix: String): Option[Map[String, String]] = {
    val m = regex.pattern.matcher(src)
    if (m.matches) {
      var matches = Map[String, String]()
      (1 to m.groupCount).foreach(ix => {
        matches += groupPrefix + ix -> m.group(ix)
      })
      Some(matches)
    } else None
  }

  def apply(request: HttpServletRequest): Option[Map[String, String]]

}

case class UriRegexMatcher(val uriRegex: String) extends RequestMatcher {

  def apply(request: HttpServletRequest) =
    extractMatches(uriRegex.r, URLDecoder.decode(request.getRequestURI, "UTF-8"), "uri$")

}

case class HeadersRegexMatcher(val criteria: (String, String)*) extends RequestMatcher {

  def apply(request: HttpServletRequest): Option[Map[String, String]] = {
    var params = Map[String,String]()
    criteria.toList.foreach(
      crit => matchHeader(crit._1, request.getHeader(crit._1), crit._2.r) match {
        case Some(p) => params ++= p
        case _ => return None
      })
    Some(params)
  }

  def matchHeader(headerName: String, headerValue: String, crit: Regex): Option[Map[String, String]] =
    if (headerValue == null) None
    else extractMatches(crit, headerValue, headerName + "$")

}
