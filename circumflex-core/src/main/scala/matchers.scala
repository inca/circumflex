package circumflex.core


import javax.servlet.http.HttpServletRequest
import util.matching.Regex

object MatchingUtils {

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

  def extractMatches(pattern: String,
                     src: String,
                     separators:String): Option[Map[String, String]] = {
    var matches = Map[String, String]()
    def extract(patternBuffer: List[Char], srcBuffer: List[Char]): Boolean =
      patternBuffer match {
        case Nil if srcBuffer.isEmpty => true
        case '$' :: _ => {
          val paramName = patternBuffer.takeWhile(!separators.toList.contains(_)).mkString
          val paramValue = srcBuffer.takeWhile(!separators.toList.contains(_)).mkString
          if (paramName.length > 1)
            matches += (paramName.substring(1) -> paramValue)
          extract(patternBuffer.drop(paramName.length), srcBuffer.drop(paramValue.length))
        }
        case p :: restPattern => srcBuffer match {
          case s :: restSrc if p == s => extract(restPattern, restSrc)
          case _ => false
        }
        case _ => false
      }
    if (extract(pattern.toList, src.toList)) Some(matches)
    else None
  }
}

import MatchingUtils._

trait RequestMatcher {

  def matchRequest(request: HttpServletRequest): Option[Map[String, Object]]

  def apply(request: HttpServletRequest): Option[RouteContext] = matchRequest(request) match {
    case Some(params) => Some(new RouteContext(request, params))
    case _ => None
  }

  def apply(request: RouteContext): Option[RouteContext] = this(request.request) match {
    case Some(matchedRequest: RouteContext) =>
      Some(new RouteContext(request.request) ++ request.params ++ matchedRequest.params)
    case _ => None
  }

}

case class UriRegexMatcher(val uriRegex: Regex) extends RequestMatcher {
  def matchRequest(request: HttpServletRequest) =
    extractMatches(uriRegex, request.getRequestURI, "uri$")
}

case class UriPatternMatcher(val uriPattern: String) extends RequestMatcher {
  def matchRequest(request: HttpServletRequest) =
    extractMatches(uriPattern, request.getRequestURI, "/.")
}

abstract class RequestHeadersMatcher[C](val criteria: (String, C)*) extends RequestMatcher {

  def matchHeader(headerName: String, headerValue: String, crit: C): Option[Map[String,String]]

  def matchRequest(request: HttpServletRequest) = matchRequest(request, Some(Map[String,String]()), criteria.toList)

  private def matchRequest(request: HttpServletRequest,
                           params: Option[Map[String,String]],
                           chain: List[Pair[String,C]]): Option[Map[String,String]] =
    params match {
      case None => None
      case Some(_) if chain == Nil => params
      case Some(params) => {
        val headerName = chain.head._1
        val headerValue = request.getHeader(headerName)
        if (headerValue == null) return None
        matchHeader(headerName, headerValue, chain.head._2) match {
          case Some(headerParams) => Some(headerParams ++ params)
          case _ => None
        }
      }
    }
}

case class HeadersRegexMatcher(override val criteria: (String,Regex)*)
    extends RequestHeadersMatcher[Regex] {
  def matchHeader(headerName: String, headerValue: String, crit: Regex) =
    extractMatches(crit, headerValue, headerName + "$")
}

case class HeadersPatternMatcher(override val criteria: (String,String)*)
    extends RequestHeadersMatcher[String] {
  def matchHeader(headerName: String, headerValue: String, crit: String) =
    extractMatches(crit, headerValue, ":,; /")
}