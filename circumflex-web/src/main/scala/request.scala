package ru.circumflex.web

import javax.servlet.http.HttpServletRequest
import collection.immutable.Map
import ru.circumflex.core._
import java.util.Date
import collection.Iterator

/*!# HTTP Request

The `HttpRequest` class wraps specified `raw` HTTP servlet request and allows you to
use core Scala classes to operate with Servlet API.

This class is designed to efficiently cover mostly used methods of `HttpServletRequest`,
however, you still can access the `raw` field, which holds actual request.
*/

class HttpRequest(val raw: HttpServletRequest) {

  /*!# Request Basics

  General request information can be accessed using following methods:

    * `method` returns the name of HTTP method with which the request was made;
    * `scheme` returns the name of the scheme used to make this request (e.g. "http", "https"
    or "ftp");
    * `uri` returns the request URI without query string;
    * `queryString` returns the query string that is contained in the request URL after the path;
    * `url` reconstructs an URL the client used to make the request;
    * `sessionId` returns session identifier specified by the client;
    * `secure_?` returns `true` if the request was made using a secure channel, such as HTTPS;
    *
  */
  def method = raw.getMethod
  def scheme = raw.getScheme
  def uri = raw.getRequestURI
  def queryString = raw.getQueryString
  def url = raw.getRequestURL
  def sessionId = raw.getRequestedSessionId
  def secure_?() = raw.isSecure

  /*!# Server Information

  Following methods provide information about the server:

    * `serverHost` returns the host name of the server for which this request is originated;
    * `serverPort` returns the port number to which the request was sent.
  */
  def serverHost: String = raw.getServerName
  def serverPort: String = raw.getServerPort

  /*!# Client Information

  Following methods provide information about the client wich sent this request.
  */

  /**
   * If the request is authenticated, return the login of user making this request. Otherwise,
   * return `None`.
   */
  def clientLogin: Option[String] = raw.getRemoteUser

  /**
   * Returns the Internet Protocol address of the client or last proxy that sent this request.
   */
  def clientIp: String = raw.getRemoteAddr

  /**
   * Returns the fully-qualified host name of the client or last proxy that sent this request.
   */
  def clientHost: String = raw.getRemoteHost

  /*!## Cookies

  The `cookies` field provides access to request cookies. You can use rich functionality
  of Scala collections to work with cookies:

      request.cookies.find(_.name == "my.cookie")
      request.cookies.filter(_.secure)
      request.cookies.groupBy(_.maxAge)
  */
  lazy val cookies: Seq[HttpCookie] = raw.getCookies.map(c => HttpCookie(c))

  /*!## Headers

  Request headers are accessible via the `headers` field. It is immutable Scala map,
  from `String` to `String`, thus you can use all Scala fancies to work with request
  headers.

  Note that HTTP specification allows clients to send multiple header values using
  comma-delimited strings. To read multiple values from header use `split`:

      val accepts = request.headers('Accept).split(",")
  */
  val headers: Map[String, String] = new Map[String, String]() {
    def +[B1 >: String](kv: (String, B1)): Map[String, B1] = this
    def -(key: String): Map[String, String] = this
    def iterator: Iterator[(String, String)] =
      new EnumerationIterator[String](raw.getHeaderNames)
          .map(n => (n -> raw.getHeader(n)))
    def get(key: String): Option[String] = raw.getHeader(key)
    def getAsMillis(key: String): Option[Long] = raw.getDateHeader(key)
    def getAsDate(key: String): Option[Long] = getAsMillis(key).map(new Date(_))
    def getAsInt(key: String): Option[Long] = raw.getIntHeader(key)
  }

}