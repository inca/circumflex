package ru.circumflex.web

import collection.immutable.Map
import ru.circumflex.core._
import java.util.Date
import collection.Iterator
import javax.servlet.http.{HttpSession, HttpSession, HttpServletRequest}
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
    * `secure_?` returns `true` if the request was made using a secure channel, such as HTTPS.
  */
  def method = raw.getMethod
  def scheme = raw.getScheme
  def uri = raw.getRequestURI
  def queryString = raw.getQueryString
  def url = raw.getRequestURL
  def secure_?() = raw.isSecure

  /*!# Client & Server Information

  Following methods provide information about the server:

    * `serverHost` returns the host name of the server for which this request was originated;
    * `serverPort` returns the port number to which the request was sent;
    * `localIp` returns the Internet Protocol address of the interface on which the request was
    received;
    * `localHost` returns the host name of the IP interface on which the request was received;
    * `localPort` returns the IP port on which the request was received.

  Following methods can be used to retrieve basic information about the client:

    * `remoteIp` returns the Internet Protocol address of the client (or last proxy) that sent
    the request;
    * `remoteHost` returns the host name of the client (or last proxy) that sent the request;
    * `remoteLogin` returns the login of the user making the request wrapped in `Some`, if the
    user has been authenticated, or `None` if the user has not been authenticated;
    * `sessionId` returns session identifier specified by the client;
  */
  def serverHost: String = raw.getServerName
  def serverPort: String = raw.getServerPort
  def localIp: String = raw.getLocalAddr
  def localHost: String = raw.getLocalName
  def localPort: String = raw.getLocalPort

  def remoteIp: String = raw.getRemoteAddr
  def remoteHost: String = raw.getRemoteHost
  def remoteLogin: Option[String] = raw.getRemoteUser
  def sessionId = raw.getRequestedSessionId

  /*!## Cookies

  The `cookies` field provides access to request cookies. You can use rich functionality
  of Scala collections to work with cookies:

      request.cookies.find(_.name == "my.cookie")
      request.cookies.filter(_.secure)
      request.cookies.groupBy(_.maxAge)
  */
  lazy val cookies: Seq[HttpCookie] = raw.getCookies.map(c => HttpCookie(c))

  /*!## Headers

  Request headers contain operational information about the requst.

  Circumflex Web Framework lets you access request headers via the `headers` field.

  Note that HTTP specification allows clients to send multiple header values using
  comma-delimited strings. To read multiple values from header use `split`:

      val accepts = request.headers('Accept).split(",")
  */
  val headers: Map[String, String] = new Map[String, String]() { map =>
    def +[B1 >: String](kv: (String, B1)): Map[String, B1] = map
    def -(key: String): Map[String, String] = map
    def iterator: Iterator[(String, String)] =
      new EnumerationIterator[String](raw.getHeaderNames)
          .map(k => (k -> raw.getHeader(k)))
    def get(key: String): Option[String] = raw.getHeader(key)
    def getAsMillis(key: String): Option[Long] = raw.getDateHeader(key)
    def getAsDate(key: String): Option[Long] = getAsMillis(key).map(new Date(_))
    def getAsInt(key: String): Option[Long] = raw.getIntHeader(key)
  }

  /*!## Attributes

  Request attributes presented by Servlet API are typically used to pass information
  between a servlet and the servlet container or between collaborating servlets.

  Circumflex Web Framework let's you access request attributes via the `attrs`
  field.
  */
  val attrs: Map[String, Any] = new Map[String, Any]() { map =>
    def +[B1 >: Any](kv: (String, B1)): Map[String, B1] = {
      raw.setAttribute(kv._1, kv._2)
      map
    }
    def -(key: String): Map[String, Any] = {
      raw.removeAttribute(key)
      map
    }
    def iterator: Iterator[(String, Any)] =
      new EnumerationIterator[String](raw.getAttributeNames)
          .map(k => (k -> raw.getAttribute(k)))
    def get(key: String): Option[Any] = raw.getAttribute(key)
  }

  /*!## Session

  Session is a convenient in-memory storage presented by Servlet API which allows web
  applications to maintain state of their clients.

  A special identifier, session ID, is generated once the session is initiated.
  Clients then, to identify themselves within application, send session ID as a cookie
  with every request.

  Circumflex Web Framework let's you access session attributes via the `session` field.

  Note that if session was not already created for the request, it will only be created
  if you attempt to add an attribute into it via `update` or `+` method, all other methods
  will return empty values without implicitly creating a session.
  */
  val session: Map[String, Any] = new Map[String, Any]() { map =>
    def +[B1 >: Any](kv: (String, B1)): Map[String, B1] = {
      raw.getSession(true).setAttribute(kv._1, kv._2)
      map
    }
    def -(key: String): Map[String, Any] = {
      val s: HttpSession = raw.getSession(false)
      if (s != null) s.removeAttribute(key)
      map
    }
    def iterator: Iterator[(String, Any)] = {
      val s: HttpSession = raw.getSession(false)
      if (s != null)
        new EnumerationIterator[String](s.getAttributeNames)
            .map(k => (k -> s.getAttribute(k)))
      else Iterator.empty
    }
    def get(key: String): Option[Any] = {
      val s: HttpSession = raw.getSession(false)
      if (s != null) s.getAttribute(name)
      else None
    }
  }

  

}