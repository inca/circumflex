package ru.circumflex

import core._
import javax.servlet.{FilterChain, FilterConfig}
import collection.Iterator
import javax.activation.MimetypesFileTypeMap
import java.io._
import org.apache.commons.io.IOUtils
import collection.mutable.Map

/*!# The `web` Package

Package `web` contains different shortcuts, utilities and helpers --
the basis of routing DSL of Circumflex Web Framework.

You should import this package to use Circumflex Web Framework in your application:

    import ru.circumflex.web._

If you don't wish to import all helpers into your global scope, then import this
package under an alias:

    import ru.circumflex.{web => cx}    // import under alias "cx"
    cx.request                          // access members
*/
package object web {

  val WEB_LOG = new Logger("ru.circumflex.web")

  /*!# Heplers

  Helpers are tiny methods and structures which provide common functionality
  for web applications like accessing current request, response, session, headers, cookies
  and other stuff.
   */
  def request = ctx("cx.request").asInstanceOf[HttpRequest]
  def response = ctx("cx.response").asInstanceOf[HttpResponse]
  def filterChain = ctx("cx.filterChain").asInstanceOf[FilterChain]
  def filterConfig = cx("cx.filterConfig").asInstanceOf[FilterConfig]
  def servletContext = filterConfig.getServletContext
  def session = request.session

  /*!## The `headers` Helper

  The `headers` object of package `ru.circumflex.web` lets you access request
  headers and set response headers (i.e. mix functionality of `request.headers`
  and `response.headers`).
  */
  object headers extends Map[String, String] {
    def +=(kv: (String, String)): this.type = {
      response.headers + kv
      this
    }
    def -=(key: String): this.type = {
      response.headers - key
      this
    }
    def iterator: Iterator[(String, String)] = request.headers.iterator
    def get(key: String): Option[String] = request.headers.get(key)
  }

  /*!## The `cookies` Helper

  The `cookies` object of package `ru.circumflex.web` lets you access request
  cookies and set response cookies (i.e. mix functionality of `request.cookies`
  and `response.cookies`).
  */
  object cookies extends Map[String, HttpCookie] {
    def +=(kv: (String, HttpCookie)): this.type = {
      response.cookies += kv._2.asInstanceOf[HttpCookie]
      this
    }
    def -=(key: String): this.type = {
      response.cookies.find(_.name == key).map(c => response.cookies -= c)
      this
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
  object flash extends Map[String, Any] with UntypedContainer {
    val SESSION_KEY = "cx.flash"
    protected def flashMap = session
        .getOrElse(SESSION_KEY, Map[String, Any]())
        .asInstanceOf[Map[String, Any]]
    def +=(kv: (String, Any)): this.type = {
      session(SESSION_KEY) = flashMap + (kv)
      this
    }
    def -=(key: String): this.type = {
      session(SESSION_KEY) = flashMap - key
      this
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

  /*!## The `param` Helper

  The `param` object of package `ru.circumflex.web` is a convenient helper to
  retrieve the parameters of current match or current request. The parameters
  are first resolved from `MatchResult` objects found in context. If no
  match result contain a parameter with specified name, then the parameter is
  resolved from request parameters.
  */
  object param extends Map[String, String] {
    def +=(kv: (String, String)): this.type = this
    def -=(key: String): this.type = this
    def iterator: Iterator[(String, String)] = ctx.iterator.flatMap(p => p._2 match {
      case m: MatchResult => m.params.iterator
      case s: String => Seq(p._1 -> s).iterator
      case _ => Iterator.empty
    }) ++ request.params.iterator
    def get(key: String): Option[String] = iterator.find(_._1 == key).map(_._2)
    override def default(key: String): String = ""
    def list(key: String): Seq[String] = iterator.filter(_._1 == key).map(_._2).toList
  }

  /*!## Response Helpers

  Circumflex Web Framework provides following helpers for sending standard
  HTTP responses:

    * `send` writes specified `text` to response buffer and, if specified,
    sets `statusCode`;
    * `sendError` sends an error to the client using specified `statusCode` and
    `message`;
    * `sendRedirect` sends `302 MOVED TEMPORARILY` to the client using specified
    `url` and optional `flashes`;
    * `sendFile` sends specified `file` to the client; if `filename` is provided,
    `Content-Disposition: attachment` is also added to the response with specified
    `filename`;
    * `xSendFile` delegates filesending to the web server; refer to documentation
    of your web server to understand how it works;
    * `sendStream` accepts a function, which uses `OutputStream` to send binary
    data;
    * `sendChars` accepts a function, which uses `Writer` to send character data;
    * `forward` delegates further request processing to another component located
    at specified `url` and immediately flushes the response at the end; note that
    if you want to forward the request to another Circumflex route, you must make
    sure that `CircumflexFilter` is mapped with `<dispatcher>FORWARD</dispatcher>`
    in `web.xml`;
    * `pass()` sends request and response down the filter chain and then immediately
    flushes response.

  All helpers by convention throw `ResponseSentMarker` which is caught by
  `CircumflexFilter` to indicate that the response have been processed
  successfully.
  */
  def send(text: String = "", statusCode: Int = -1): Nothing = {
    if (statusCode != -1)
      response.statusCode(statusCode)
    response.body(r => r.getWriter.write(text)).flush()
  }
  def sendError(statusCode: Int, message: String = "No message available."): Nothing =
    response.body(r => r.sendError(statusCode, message)).flush()
  def sendRedirect(url: String, flashes: (String, Any)*): Nothing = {
    flashes.foreach(kv => flash(kv._1) = kv._2)
    response.body(r => r.sendRedirect(url)).flush()
  }
  def sendFile(file: File, filename: String = ""): Nothing = {
    // if filename is provided, add `Content-Disposition` header
    if (filename != "") response.attachment(filename)
    // if not set explicitly, infer content type from extension
    if (response.contentType == "")
      response.contentType(new MimetypesFileTypeMap().getContentType(file))
    // send file by copying streams
    response.body { r =>
      val is = new FileInputStream(file)
      try {
        IOUtils.copy(is, r.getOutputStream)
      } finally {
        is.close()
      }
    } flush()
  }
  def xSendFile(file: File, filename: String = ""): Nothing = {
    // if filename is provided, add `Content-Disposition` header
    if (filename != "") response.attachment(filename)
    val xsf = cx.instantiate[XSendFileHeader]("cx.xSendFile", DefaultXSendFileHeader)
    response.headers(xsf.name) = xsf.value(file)
    send()
  }
  def sendStream(streamFunc: OutputStream => Unit): Nothing =
    response.body(r => streamFunc(r.getOutputStream)).flush()
  def sendChars(writerFunc: Writer => Unit): Nothing =
    response.body(r => writerFunc(r.getWriter)).flush()
  def forward(url: String): Nothing = {
    request.forward(url)
    response.flush()
  }
  def pass(): Nothing = {
    filterChain.doFilter(request.raw, response.raw)
    response.flush()
  }

  /*!## The `matchers` Helper

  The `matchers` helper contains shortcuts for various matchers (for example, by known HTTP headers).
  You should import this object if you want to use it:

      import ru.circumflex.web.{matchers => m}

      get("/" & m.ACCEPT(":mime")) = "You are accepting " + param("mime")
  */
  object matchers {
    val ACCEPT = new HeaderMatcherHelper("Accept")
    val ACCEPT_CHARSET = new HeaderMatcherHelper("Accept-Charset")
    val ACCEPT_ENCODING = new HeaderMatcherHelper("Accept-Encoding")
    val ACCEPT_LANGUAGE = new HeaderMatcherHelper("Accept-Language")
    val ACCEPT_RANGES = new HeaderMatcherHelper("Accept-Ranges")
    val AUTHORIZARION = new HeaderMatcherHelper("Authorization")
    val CACHE_CONTROL = new HeaderMatcherHelper("Cache-Control")
    val CONNECTION = new HeaderMatcherHelper("Connection")
    val CONTENT_LENGTH = new HeaderMatcherHelper("Content-Length")
    val COOKIE = new HeaderMatcherHelper("Cookie")
    val CONTENT_TYPE = new HeaderMatcherHelper("Content-Type")
    val DATE = new HeaderMatcherHelper("Date")
    val EXPECT = new HeaderMatcherHelper("Expect")
    val FROM = new HeaderMatcherHelper("From")
    val HOST = new HeaderMatcherHelper("Host")
    val IF_MATCH = new HeaderMatcherHelper("If-Match")
    val IF_MODIFIED_SINCE = new HeaderMatcherHelper("If-Modified-Since")
    val IF_NONE_MATCH = new HeaderMatcherHelper("If-None-Match")
    val IF_RANGE = new HeaderMatcherHelper("If-Range")
    val IF_UNMODIFIED_SINCE = new HeaderMatcherHelper("If-Unmodified-Since")
    val MAX_FORWARDS = new HeaderMatcherHelper("Max-Forwards")
    val PRAGMA = new HeaderMatcherHelper("Pragma")
    val PROXY_AUTHORIZATION = new HeaderMatcherHelper("Proxy-Authorization")
    val RANGE = new HeaderMatcherHelper("Range")
    val REFERER = new HeaderMatcherHelper("Referer")
    val TE = new HeaderMatcherHelper("TE")
    val UPGRADE = new HeaderMatcherHelper("Upgrade")
    val USER_AGENT = new HeaderMatcherHelper("User-Agent")
    val VIA = new HeaderMatcherHelper("Via")
    val WARNING = new HeaderMatcherHelper("Warning")
  }

}
