package ru.circumflex

import ru.circumflex.core._
import org.slf4j.LoggerFactory
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
/*!# The `web` Package

Package `web` contains different utilities and implicits -- the basis of
routing DSL of Circumflex Web Framework.

You should import this package to use Circumflex Web Framework in your application:

    import ru.circumflex.web._
*/

package object web {

  implicit def string2paramHelper(str: String): ParamHelper = new ParamHelper(str)
  implicit def symbol2paramHelper(sym: Symbol): ParamHelper = string2paramHelper(sym.name)

  val CX_WEB_LOG = LoggerFactory.getLogger("ru.circumflex.web")

  def request = ctx('request).asInstanceOf[HttpServletRequest]
  def response = ctx('response).asIstanceOf[HttpServletResponse]

  /*
  val Accept = new HeaderMatcherHelper("Accept")
  val AcceptCharset = new HeaderMatcherHelper("Accept-Charset")
  val AcceptEncoding = new HeaderMatcherHelper("Accept-Encoding")
  val AcceptLanguage = new HeaderMatcherHelper("Accept-Language")
  val AcceptRanges = new HeaderMatcherHelper("Accept-Ranges")
  val Authorization = new HeaderMatcherHelper("Authorization")
  val CacheControl = new HeaderMatcherHelper("Cache-Control")
  val ContentLength = new HeaderMatcherHelper("Content-Length")
  val ContentType = new HeaderMatcherHelper("Content-Type")
  val HeaderDate = new HeaderMatcherHelper("Date")
  val Expect = new HeaderMatcherHelper("Expect")
  val Host = new HeaderMatcherHelper("Host")
  val IfMatch = new HeaderMatcherHelper("If-Match")
  val IfModifiedSince = new HeaderMatcherHelper("If-Modified-Since")
  val IfNoneMatch = new HeaderMatcherHelper("If-None-Match")
  val IfRange = new HeaderMatcherHelper("If-Range")
  val IfUnmodifiedSince = new HeaderMatcherHelper("If-Unmodified-Since")
  val MaxForwards = new HeaderMatcherHelper("Max-Forwards")
  val Pragma = new HeaderMatcherHelper("Pragma")
  val ProxyAuthorization = new HeaderMatcherHelper("Proxy-Authorization")
  val Referer = new HeaderMatcherHelper("Referer")
  val Upgrade = new HeaderMatcherHelper("Upgrade")
  val UserAgent = new HeaderMatcherHelper("User-Agent")
  val Via = new HeaderMatcherHelper("Via")
  */
}
