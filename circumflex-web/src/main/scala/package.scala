package ru.circumflex

import ru.circumflex.core._
import javax.servlet.{FilterChain, FilterConfig}

/*!# The `web` Package

Package `web` contains different shortcuts, utilities and implicits -- the basis of
routing DSL of Circumflex Web Framework.

You should import this package to use Circumflex Web Framework in your application:

    import ru.circumflex.web._
*/
package object web {

  val CX_WEB_LOG = new Logger("ru.circumflex.web")

  def request = ctx("cx.request").asInstanceOf[HttpRequest]
  def response = ctx("cx.response").asInstanceOf[HttpResponse]
  def filterConfig = cx("cx.filterConfig").asInstanceOf[FilterConfig]
  def filterChain = cx("cx.filterChain").asInstanceOf[FilterChain]

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
