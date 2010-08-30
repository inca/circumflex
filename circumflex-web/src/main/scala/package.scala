package ru.circumflex

import ru.circumflex.core._
import javax.servlet.{FilterChain, FilterConfig}
import collection.immutable.Map
import collection.Iterator

/*!# The `web` Package

Package `web` contains different shortcuts, utilities, helpers and implicits --
the basis of routing DSL of Circumflex Web Framework.

You should import this package to use Circumflex Web Framework in your application:

    import ru.circumflex.web._
*/
package object web {

  val WEB_LOG = new Logger("ru.circumflex.web")

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

  /*!## The `headers` Helper

  The `headers` object of package `ru.circumflex.web` lets you access request
  headers and set response headers (i.e. mix functionality of `request.headers`
  and `response.headers`).
  */
  object headers extends Map[String, String] {
    def +[B1 >: String](kv: (String, B1)): Map[String, B1] = response.headers + kv
    def -(key: String): Map[String, String] = response.headers - key
    def iterator: Iterator[(String, String)] = request.headers.iterator
    def get(key: String): Option[String] = request.headers.get(key)
  }

  /*!# Heplers

  Helpers are tiny methods and structures which provide common functionality
  for web applications like accessing current request, response, session, headers, cookies
  and other stuff.
   */
  def request = ctx("cx.request").asInstanceOf[HttpRequest]
  def response = ctx("cx.response").asInstanceOf[HttpResponse]
  def filterConfig = cx("cx.filterConfig").asInstanceOf[FilterConfig]
  def filterChain = cx("cx.filterChain").asInstanceOf[FilterChain]
  def session = request.session

  /*!## The `cookies` Helper

  The `cookies` object of package `ru.circumflex.web` lets you access request
  cookies and set response cookies (i.e. mix functionality of `request.cookies`
  and `response.cookies`).
  */
  object cookies extends Map[String, HttpCookie] {
    def +[B1 >: HttpCookie](kv: (String, B1)): Map[String, B1] = {
      response.cookies += kv._2
      return this
    }
    def -(key: String): Map[String, HttpCookie] = {
      response.cookies.find(_.name == key).map(c => response.cookies -= c)
      return this
    }
    def iterator: Iterator[(String, HttpCookie)] =
      request.cookies.iterator.map(c => (c.name -> c))
    def get(key: String): Option[HttpCookie] =
      request.cookies.find(c => c.name == key)
  }

  /*!## The `flash` Helper

  The `flash` object provides a way to pass temporary objects between requests.
  Flash variables are stored in session until first access.
  */
  object flash extends Map[String, Any] {
    val SESSION_KEY = "cx.flash"
    protected def flashMap = session
        .getOrElse(SESSION_KEY, Map[String, Any]())
        .asInstanceOf[Map[String, Any]]

    def +[B1 >: Any](kv: (String, B1)): Map[String, B1] = {
      session(SESSION_KEY) = flashMap + (kv)
      return this
    }
    def -(key: String): Map[String, Any] = {
      session(SESSION_KEY) = flashMap - key
      return this
    }
    def iterator: Iterator[(String, Any)] = flashMap.iterator
    def get(key: String): Option[Any] = {
      val m = flashMap
      flashMap.get(key) map { v =>
        session(SESSION_KEY) = m - key
        v
      }
    }
    override def contains(key: String): Boolean = flashMap.contains(key)
  }

}
