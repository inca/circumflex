package ru.circumflex.core

import java.io.File

case class RouteMatchedException(val response: Option[HttpResponse]) extends Exception

/* ## Request Router */

class RequestRouter {

  implicit def textToResponse(text: String): HttpResponse = TextResponse(text)
  implicit def requestRouterToResponse(router: RequestRouter): HttpResponse = error(404)

  /* ### Routes */

  val get = new Route("get")
  val getOrPost = new Route("get", "post")
  val getOrHead = new Route("get", "head")
  val post = new Route("post")
  val put = new Route("put")
  val delete = new Route("delete")
  val head = new Route("head")
  val options = new Route("options")
  val any = new Route("get", "post", "put", "delete", "head", "options")

  /* ### Matchers */

  def headers(crit: (String, StringMatcher)*) = new HeaderRequestMatcher(crit : _*)
  implicit def pair2String_StringMatcher[A <% String, B <% StringMatcher]
    (p: (A, B)): (String, StringMatcher) = (p._1, p._2)
  
  /* ### Context shortcuts */

  def header = ctx.header
  def session = ctx.session
  def flash = ctx.flash
  def uri: Match = matching("uri")

  /* ### Helpers */

  /**
   * Determines, if the request is XMLHttpRequest (for AJAX applications).
   */
  def isXhr = header("X-Requested-With") match {
    case Some("XMLHttpRequest") => true
    case _ => false
  }

  /**
   * Rewrites request URI. Normally it causes the request to travel all the way through
   * the filters and `RequestRouter` once again but with different URI.
   * You must use this method with caution to prevent infinite loops.
   * You must also add `<dispatcher>FORWARD</dispatcher>` to filter mapping to
   * allow request processing with certain filters.
   */
  def rewrite(target: String): Nothing = {
    ctx.request.getRequestDispatcher(target).forward(ctx.request, ctx.response)
    throw RouteMatchedException(None)
  }

  /**
   * Retrieves a String parameter from context.
   */
  def param(key: String): Option[String] = ctx.stringParam(key)

  /**
   * Retrieves a Match from context.
   * Since route matching has success it supposes the key existence.
   */
  def matching(key: String): Match = ctx.matchParam(key).get

  /**
   * Sends error with specified status code and message.
   */
  def error(errorCode: Int, message: String) = ErrorResponse(errorCode, message)

  /**
   * Sends error with specified status code.
   */
  def error(errorCode: Int) = ErrorResponse(errorCode, "no message available")

  /**
   * Sends a `302 Moved Temporarily` redirect (with optional flashes).
   */
  def redirect(location: String, flashes: (String, Any)*) = {
    for ((key, value) <- flashes) flash(key) = value
    RedirectResponse(location)
  }

  /**
   * Sends empty `200 OK` response.
   */
  def done: HttpResponse = done(200)

  /**
   * Sends empty response with specified status code.
   */
  def done(statusCode: Int): HttpResponse = {
    ctx.statusCode = statusCode
    new EmptyResponse
  }

  /**
   * Immediately stops processing with `400 Bad Request` if one of specified
   * parameters is not provided.
   */
  def requireParams(names: String*) = names.toList.foreach(name => {
    if (param(name) == None)
      throw new RouteMatchedException(Some(error(400, "Missing " + name + " parameter.")))
  })

  /**
   * Sends a file.
   */
  def sendFile(file: File): FileResponse = FileResponse(file)

  /**
   * Sends a file with Content-Disposition: attachment with specified UTF-8 filename.
   */
  def sendFile(file: File, filename: String): FileResponse = {
    attachment(filename)
    sendFile(file)
  }

  /**
   * Sends a file with the help of Web server using X-SendFile feature.
   */
  def xSendFile(file: File): HttpResponse = {
    val xsf = Circumflex.XSendFileHeader
    header(xsf.name) = xsf.value(file)
    done
  }

  /**
   * Sends a file with the help of Web server using X-SendFile feature, also setting
   * Content-Disposition: attachment with specified UTF-8 filename.
   */
  def xSendFile(file: File, filename: String): HttpResponse = {
    attachment(filename)
    xSendFile(file)
  }

  /**
   * Sets content type header.
   */
  def contentType(ct: String): this.type = {
    ctx.contentType = ct
    return this
  }

  /**
   * Adds an Content-Disposition header with "attachment" content and specified UTF-8 filename.
   */
  def attachment(filename: String): this.type = {
    header("Content-Disposition") =
        "attachment; filename=\"" + new String(filename.getBytes("UTF-8"), "ISO-8859-1") + "\""
    return this
  }

  /* ## Request extractors */
  
  object Uri {
    def unapplySeq(ctx: CircumflexContext): Option[Seq[String]] =
      Some(ctx.matchParam("uri").get.toSeq)
  }

  case class HeaderExtractor(name: String) {
    def unapplySeq(ctx: CircumflexContext): Option[Seq[String]] =
      ctx.matchParam(name) match {
        case Some(m) => Some(m.toSeq)
        case None    => ctx.header(name) map { List(_) }
      }
  }

  val Accept = HeaderExtractor("Accept")
  val AcceptCharset = HeaderExtractor("Accept-Charset")
  val AcceptEncoding = HeaderExtractor("Accept-Encoding")
  val AcceptLanguage = HeaderExtractor("Accept-Language")
  val AcceptRanges = HeaderExtractor("Accept-Ranges")
  val Authorization = HeaderExtractor("Authorization")
  val CacheControl = HeaderExtractor("Cache-Control")
  val Connection = HeaderExtractor("Connection")
  val Cookie = HeaderExtractor("Cookie")
  val ContentLength = HeaderExtractor("Content-Length")
  val ContentType = HeaderExtractor("Content-Type")
  val HeaderDate = HeaderExtractor("Date")
  val Expect = HeaderExtractor("Expect")
  val From = HeaderExtractor("From")
  val Host = HeaderExtractor("Host")
  val IfMatch = HeaderExtractor("If-Match")
  val IfModifiedSince = HeaderExtractor("If-Modified-Since")
  val IfNoneMatch = HeaderExtractor("If-None-Match")
  val IfRange = HeaderExtractor("If-Range")
  val IfUnmodifiedSince = HeaderExtractor("If-Unmodified-Since")
  val MaxForwards = HeaderExtractor("Max-Forwards")
  val Pragma = HeaderExtractor("Pragma")
  val ProxyAuthorization = HeaderExtractor("Proxy-Authorization")
  val Range = HeaderExtractor("Range")
  val Referer = HeaderExtractor("Referer")
  val TE = HeaderExtractor("TE")
  val Upgrade = HeaderExtractor("Upgrade")
  val UserAgent = HeaderExtractor("User-Agent")
  val Via = HeaderExtractor("Via")
  val War = HeaderExtractor("War")
}

/**
 * ## Route
 *
 * Dispatches current request if it passes all matchers.
 * Common matchers are based on HTTP methods, URI and headers.
 */
class Route(val matchingMethods: String*) {

  protected def dispatch(response: =>HttpResponse, matchers: RequestMatcher*): Unit =
    matchingMethods.find(ctx.method.equalsIgnoreCase(_)) match {
      case Some(_) => {
        var params = Map[String, Match]()
        matchers.toList.foreach(rm => rm(ctx.request) match {
          case None => return
          case Some(p) => params ++= p
        })
        // All matchers succeeded
        ctx._matches = params
        throw RouteMatchedException(Some(response))
      } case _ =>
    }

  /**
   * For syntax "get(...) { case Extractors(...) => ... }"
   */
  def apply(matcher: StringMatcher)(f: CircumflexContext => HttpResponse): Unit =
    dispatch(ContextualResponse(f), new UriRequestMatcher(matcher))

  def apply(matcher: StringMatcher, matcher1: RequestMatcher)(f: CircumflexContext => HttpResponse): Unit =
    dispatch(ContextualResponse(f), new UriRequestMatcher(matcher), matcher1)

  /**
   * For syntax "get(...) = response"
   */
  def update(matcher: StringMatcher, response: =>HttpResponse): Unit =
    dispatch(response, new UriRequestMatcher(matcher))

  def update(matcher: StringMatcher, matcher1: RequestMatcher, response: =>HttpResponse): Unit =
    dispatch(response, new UriRequestMatcher(matcher), matcher1)
}

/* ## Helpers */

/**
 * A helper for getting and setting response headers in a DSL-like way.
 */
class HeadersHelper extends HashModel {

  def get(key: String) = apply(key)

  def apply(name: String): Option[String] = {
    val value = ctx.request.getHeader(name)
    if (value == null) None
    else Some(value)
  }

  def update(name: String, value: String) = ctx.stringHeaders += name -> value

  def update(name: String, value: Long) = ctx.dateHeaders += name -> value

  def update(name: String, value: java.util.Date) = ctx.dateHeaders += name -> value.getTime

}

/**
 * A helper for getting and setting session-scope attributes.
 */
class SessionHelper extends HashModel {

  def get(key: String) = apply(key)

  def apply(name: String): Option[Any] = {
    val value = ctx.request.getSession.getAttribute(name)
    if (value == null) None
    else Some(value)
  }

  def update(name: String, value: Any) = ctx.request.getSession.setAttribute(name, value)

}