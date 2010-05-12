package ru.circumflex.core

import java.io.File
import util.matching.Regex

class RouteMatchedException(val response: HttpResponse) extends Exception

// ## Request Router

class RequestRouter(val prefix: String = "") {

  implicit def textToResponse(text: String): HttpResponse = TextResponse(text)
  implicit def requestRouterToResponse(router: RequestRouter): HttpResponse = error(404)

  implicit def string2uriMatcher(str: String): RegexMatcher =
    new RegexMatcher("uri", prefix + context.uri, str)

  implicit def regex2uriMatcher(regex: Regex): RegexMatcher =
    new RegexMatcher("uri", prefix + context.uri, regex)

  /**
   * ## Route
   *
   * Dispatches current request if it passes all matchers.
   * Common matchers are based on HTTP methods, URI and headers.
   */
  class Route(matchingMethods: String*) {

    protected def dispatch(matcher: Matcher, response: =>HttpResponse): Unit =
      matchingMethods.find(context.method.equalsIgnoreCase(_)) match {
        case Some(_) =>
          matcher.apply() match {
            case None => return
            case Some(matches: Seq[Matcher]) =>
              matches.foreach(m => context += m.name -> m)
              throw new RouteMatchedException(response)
          }
        case _ =>
      }

    /**
     * For syntax "get(...) = response"
     */
    def update(matcher: Matcher, response: =>HttpResponse): Unit =
      dispatch(matcher, response)

  }

  // ### Routes

  val get = new Route("get")
  val getOrPost = new Route("get", "post")
  val getOrHead = new Route("get", "head")
  val post = new Route("post")
  val put = new Route("put")
  val delete = new Route("delete")
  val head = new Route("head")
  val options = new Route("options")
  val any = new Route("get", "post", "put", "delete", "head", "options")

  // ### Context shortcuts

  def context = CircumflexContext.context
  def header = context.header
  def session = context.session
  def flash = context.flash

  def uri: Match = context.apply("uri") match {
    case Some(m: Match) => m
    case None => new Match("uri", "splat" -> context.uri)
  }

  // ### Helpers

  /**
   * Determines, if the request is XMLHttpRequest (for AJAX applications).
   */
  def isXhr = header("X-Requested-With").getOrElse("") == "XMLHttpRequest"

  /**
   * Retrieves a String from context.
   */
  def param(key: String): Option[String] = context.getString(key)

  /**
   * Sends error with specified status code and message.
   */
  def error(errorCode: Int, message: String = "no message available"): Nothing =
    throw new RouteMatchedException(ErrorResponse(errorCode, message))

  /**
   * Rewrites request URI. Normally it causes the request to travel all the way through
   * the filters and `RequestRouter` once again but with different URI.
   * You must use this method with caution to prevent infinite loops.
   * You must also add `<dispatcher>FORWARD</dispatcher>` to filter mapping to
   * allow request processing with certain filters.
   */
  def rewrite(target: String): Nothing = {
    context.request.getRequestDispatcher(target).forward(context.request, context.response)
    throw new RouteMatchedException(EmptyResponse)
  }

  /**
   * Sends a `302 Moved Temporarily` redirect (with optional flashes).
   */
  def redirect(location: String, flashes: (String, Any)*): Nothing = {
    for ((key, value) <- flashes) flash(key) = value
    throw new RouteMatchedException(RedirectResponse(location))
  }

  /**
   * Sends empty response with specified status code (default is `200 OK`).
   */
  def done(statusCode: Int = 200): Nothing = {
    context.statusCode = statusCode
    throw new RouteMatchedException(EmptyResponse)
  }

  /**
   * Immediately stops processing with `400 Bad Request` if one of specified
   * parameters is not provided.
   */
  def requireParams(names: String*): Unit =
    for (name <- names if param(name).isEmpty)
      error(400, "Missing " + name + " parameter.")

  /**
   * Sends a file with Content-Disposition: attachment with specified UTF-8 filename.
   */
  def sendFile(file: File, filename: String = null): Nothing = {
    if (filename != null) attachment(filename)
    throw new RouteMatchedException(FileResponse(file))
  }

  /**
   * Sends a file with the help of Web server using X-SendFile feature, also setting
   * Content-Disposition: attachment with specified UTF-8 filename.
   */
  def xSendFile(file: File, filename: String = null): Nothing = {
    if (filename != null) attachment(filename)
    val xsf = Circumflex.newObject("cx.XSendFileHeader", DefaultXSendFileHeader)
    header(xsf.name) = xsf.value(file)
    done()
  }

  /**
   * Adds an Content-Disposition header with "attachment" content and specified UTF-8 filename.
   */
  def attachment(filename: String): this.type = {
    header("Content-Disposition") =
        "attachment; filename=\"" + new String(filename.getBytes("UTF-8"), "ISO-8859-1") + "\""
    return this
  }

  /**
   * Sets content type header.
   */
  def contentType(ct: String): this.type = {
    context.contentType = ct
    return this
  }

  /**
   * A helper to sets appropriate headers for disabling client-side caching.
   */
  def noCache() {
    header('Pragma) = "no-cache"
    header("Cache-Control") = "no-store"
    header('Expires) = 0l
  }

}