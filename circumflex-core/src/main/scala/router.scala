/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

package ru.circumflex.core

import java.io.File
import Circumflex._

case class RouteMatchedException(val response: Option[HttpResponse]) extends Exception

class RequestRouter {

  implicit def textToResponse(text: String): HttpResponse = TextResponse(text)
  implicit def requestRouterToResponse(router: RequestRouter): HttpResponse = error(404)

  def ctx = Circumflex.ctx

  /* ROUTES AND MATCHERS */

  val get = new Route("get")
  val getOrPost = new Route("get", "post")
  val getOrHead = new Route("get", "head")
  val post = new Route("post")
  val put = new Route("put")
  val delete = new Route("delete")
  val head = new Route("head")
  val options = new Route("options")
  val any = new Route("get", "post", "put", "delete", "head", "options")

  /**
   * Constructs a headers-based matcher.
   */
  def headers(crit: (String, String)*) = new HeadersRegexMatcher(crit : _*)

  /* CONTEXT SHORTCUTS */

  def header = ctx.header
  def session = ctx.session
  def flash = ctx.flash
  def uri = ctx.uri

  /**
   * Determines, if the request is XMLHttpRequest (for AJAX applications).
   */
  def isXhr = header("X-Requested-With") match {
    case Some("XMLHttpRequest") => true
    case _ => false
  }

  /**
   * Rewrites request URI. Normally it causes the request to travel all the way through
   * the filters and <code>RequestRouter</code> once again but with different URI.
   * You must use this method with caution to prevent infinite loops.
   * You must also add &lt;dispatcher&gt;FORWARD&lt;/dispatcher&gt; to filter mapping to
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
   * Sends error with specified status code and message.
   */
  def error(errorCode: Int, message: String) = ErrorResponse(errorCode, message)

  /**
   * Sends error with specified status code.
   */
  def error(errorCode: Int) = ErrorResponse(errorCode, "no message available")

  /**
   * Sends a 302 MOVED TEMPORARILY redirect (with optional flashes).
   */
  def redirect(location: String, flashes: Pair[String, Any]*) = {
    flashes.toList.foreach(p => flash(p._1) = p._2)
    RedirectResponse(location)
  }

  /**
   * Sends empty response HTTP 200 OK.
   */
  def done: HttpResponse = done(200)

  /**
   * Sends empty response with specified status code.
   */
  def done(statusCode: Int): HttpResponse = {
    ctx.statusCode = statusCode
    EmptyResponse()
  }

  /**
   * Immediately stops processing with HTTP 400 Bad Request if one of specified
   * parameters is not provided.
   */
  def requireParams(names: String*) = names.toList.foreach(name => {
    if (param(name) == None)
      throw new RouteMatchedException(Some(error(400, "Missing " + name + " parameter.")))
  })

  /**
   * Sends a file.
   */
  def sendFile(file: File) = FileResponse(file)

  /* COMMON HELPERS */

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

}

/**
 * Dispatches current request if it passes all matchers.
 * Common matchers are based on HTTP methods, URI and headers.
 */
class Route(val matchingMethods: String*) {

  protected def dispatch(response: =>HttpResponse, matchers: RequestMatcher*): Unit =
    matchingMethods.find(ctx.method.equalsIgnoreCase(_)) match {
      case Some(_) => {
        var params = Map[String, String]()
        matchers.toList.foreach(rm => rm(ctx.request) match {
          case None => return
          case Some(p) => params ++= p
        })
        // All matchers succeeded
        ctx ++= params
        throw RouteMatchedException(Some(response))
      } case _ =>
    }

  def update(uriRegex: String, response: =>HttpResponse): Unit =
    dispatch(response, new UriRegexMatcher(uriRegex))

  def update(uriRegex: String, matcher1: RequestMatcher, response: =>HttpResponse): Unit =
    dispatch(response, new UriRegexMatcher(uriRegex), matcher1)
}

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
