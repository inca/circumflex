package ru.circumflex.web

import scala.collection.immutable.Map
import scala.collection.Iterator
import scala.collection.JavaConversions._
import scala.xml._
import ru.circumflex.core._
import javax.servlet.http.{HttpServletRequest}
import java.security.Principal
import java.util.{Locale, Date}
import javax.servlet.ServletInputStream
import org.apache.commons.io.IOUtils
import org.apache.commons.fileupload._
import org.apache.commons.fileupload.servlet._
import java.io.{File, BufferedReader}
import java.net.URLDecoder
import java.lang.String
import java.util.{Enumeration => JEnumeration}


/*!# HTTP Request

The `HttpRequest` class wraps specified `raw` `HttpServletRequest` and allows you to
use core Scala classes to operate with Servlet API.

This class is designed to efficiently cover mostly used methods of `HttpServletRequest`,
however, you still can access the `raw` field, which holds actual request. For more
information refer to Java Servlet API.

Since Circumflex is UTF-friendly it will implicitly set character encoding of request body
to `UTF-8`. Feel free to change it if your application requires so.
*/

/**
 * Provides a wrapper around `HttpServletRequest`, which is still available as `raw` field.
 *
 * Most methods are self-descriptive and have direct equivalents in Servlet API, so
 * Scaladocs are omitted.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0/circumflex-web/request.scala">request.scala</a>.
 */
class HttpRequest(val raw: HttpServletRequest) {

  /*!## Request Basics

  General request information can be accessed using following methods:

    * `protocol` returns the name and version of the protocol the request uses (e.g. `HTTP/1.1`);
    * `method` returns the name of HTTP method with which the request was made, it's value is
    overriden by request paremeter `_method` to provide a workaround for browsers;
    * `scheme` returns the name of the scheme used to make this request (e.g. "http", "https"
    or "ftp");
    * `uri` returns the request URI without query string;
    * `queryString` returns the query string that is contained in the request URL after the path;
    * `url` reconstructs an URL the client used to make the request;
    * `secure_?` returns `true` if the request was made using a secure channel, such as HTTPS.

  The result of `uri`, `url` and `queryString` is decoded into UTF-8 string using `URLDecoder`.

  Also note that if the method is overriden by the `_method` parameter, the original method is
  saved in context under the `cx.originalMethod` key.
  */
  def protocol = raw.getProtocol
  def scheme = raw.getScheme
  def secure_?() = raw.isSecure
  lazy val method = params.get("_method") match {
    case Some(m) =>
      // store original method in context
      ctx.update("cx.originalMethod", raw.getMethod.toLowerCase)
      m.trim.toLowerCase
    case _ => raw.getMethod.toLowerCase
  }
  lazy val uri = URLDecoder.decode(raw.getRequestURI, "UTF-8")
  lazy val queryString = URLDecoder.decode(raw.getQueryString, "UTF-8")
  lazy val url = URLDecoder.decode(raw.getRequestURL.toString, "UTF-8")

  /*!## Client & Server Information

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
    user has been authenticated (via container-managed security), or `None` if the user has
    not been authenticated;
    * `sessionId` returns session identifier specified by the client;
    * `userPrincipal` returns `java.security.Principal` for requests authenticated with
    container-managed security mechanisms;
    * `userInRole_?` indicates whether authenticated principal has specified `role` inside
    container-managed security system.
  */
  def serverHost: String = raw.getServerName
  def serverPort: Int = raw.getServerPort
  def localIp: String = raw.getLocalAddr
  def localHost: String = raw.getLocalName
  def localPort: Int = raw.getLocalPort

  def remoteIp: String = raw.getRemoteAddr
  def remoteHost: String = raw.getRemoteHost
  def remoteLogin: Option[String] = raw.getRemoteUser
  def sessionId = raw.getRequestedSessionId

  def userPrincipal: Option[Principal] = raw.getUserPrincipal
  def userInRole_?(role: String): Boolean = raw.isUserInRole(role)

  /*!## Locale

  A list of preferred locales is specified by the value of the `Accept-Language` header of
  the request. You can access this list and the most preferred locale (with maximum relative quality
  factor) using `locales` and `locale` fields.
  */
  def locale: Locale = raw.getLocale

  lazy val locales: Seq[Locale] = raw.getLocales.asInstanceOf[JEnumeration[Locale]].toSeq

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
  Circumflex Web Framework lets you access request headers via the `headers` object.
  */
  object headers extends Map[String, String] {
    def +[B1 >: String](kv: (String, B1)): Map[String, B1] = this
    def -(key: String): Map[String, String] = this
    def iterator: Iterator[(String, String)] = raw.getHeaderNames
            .asInstanceOf[JEnumeration[String]]
            .map(k => (k -> raw.getHeader(k)))
    def get(key: String): Option[String] = raw.getHeader(key)
    def getAsMillis(key: String): Option[Long] = raw.getDateHeader(key)
    def getAsDate(key: String): Option[Date] = getAsMillis(key).map(new Date(_))
    def getAsInt(key: String): Option[Long] = raw.getIntHeader(key)
  }

  /*!## Attributes

  Request attributes presented by Servlet API are typically used to pass information
  between a servlet and the servlet container or between collaborating servlets.

  Circumflex Web Framework lets you access request attributes via the `attrs` object.
  */
  object attrs extends Map[String, Any] with UntypedContainer {
    def +[B1 >: Any](kv: (String, B1)): Map[String, B1] = {
      raw.setAttribute(kv._1, kv._2)
      return this
    }
    def -(key: String): Map[String, Any] = {
      raw.removeAttribute(key)
      return this
    }
    def iterator: Iterator[(String, Any)] = raw.getAttributeNames
            .asInstanceOf[JEnumeration[String]]
            .map(k => (k -> raw.getAttribute(k)))
    def get(key: String): Option[Any] = raw.getAttribute(key)
  }

  /*!## Parameters

  Request parameters can be accessed via the `params` object.
  */
  object params extends Map[String, String] {
    def +[B1 >: String](kv: (String, B1)): Map[String, B1] = this
    def -(key: String): Map[String, String] = this
    def iterator: Iterator[(String, String)] = raw.getParameterNames
            .asInstanceOf[JEnumeration[String]]
            .map(k => (k -> raw.getParameter(k)))
    def get(key: String): Option[String] = raw.getParameter(key)
  }

  /*!## Session

  Session is a convenient in-memory storage presented by Servlet API which allows web
  applications to maintain state of their clients.

  A special identifier, session ID, is generated once the session is initiated.
  Clients then, to identify themselves within application, send session ID as a cookie
  with every request.

  Circumflex Web Framework lets you access session attributes via the `session` object.

  Note that if session was not already created for the request, it will only be created
  if you attempt to add an attribute into it via `update` or `+` method, all other methods
  will return empty values without implicitly creating a session.
  */
  object session extends Map[String, Any] with UntypedContainer {
    def +[B1 >: Any](kv: (String, B1)): Map[String, B1] = {
      raw.getSession(true).setAttribute(kv._1, kv._2)
      return this
    }
    def -(key: String): Map[String, Any] = {
      val s = raw.getSession(false)
      if (s != null) s.removeAttribute(key)
      return this
    }
    def iterator: Iterator[(String, Any)] = {
      val s = raw.getSession(false)
      if (s != null)
        s.getAttributeNames
                .asInstanceOf[JEnumeration[String]]
                .map(k => (k -> s.getAttribute(k)))
      else Iterator.empty
    }
    def get(key: String): Option[Any] = {
      val s = raw.getSession(false)
      if (s != null) s.getAttribute(key)
      else None
    }
    def invalidate: this.type = {
      val s = raw.getSession(false)
      if (s != null) s.invalidate
      return this
    }
  }

  /*!## Body

  Circumflex Web Framework lets you access the body of the request via `body` object. Following
  methods can be used to work with request body:

    * `xhr_?` returns true if this request is XMLHttpRequest;
    * `encoding` returns or sets the name of the character encoding used in the body of the
    request (as mentioned above, we implicitly set it to `UTF-8`);
    * `multipart_?` returns `true` if the request has `multipart/form-data` content and is
    suitable for [multipart operations](#multipart);
    * `length` returns the length, in bytes, of the request body;
    * `contentType` returns the MIME type of the body of the request;
    * `reader` opens `java.io.BufferedReader` to read the request body;
    * `stream` opens `javax.servlet.ServletInputStream` to read the request body;
    * `asXml` attempts to read the request body as XML element, an exception is thrown if parse
    fails;
    * `asString` reads request body into `String` using request `encoding`.

  Note that due to limitations of Servlet API, you can only access one of `reader`, `stream`,
  `xml` or `toString` methods (and only once). An `IllegalStateException` is thrown if you
  access more than one of these methods.
  */
  object body {

    def xhr_?(): Boolean = headers.getOrElse("X-Requested-With", "") == "XMLHttpRequest"
    def multipart_?(): Boolean = ServletFileUpload.isMultipartContent(raw)
    def encoding: String = raw.getCharacterEncoding
    def encoding_=(enc: String) = raw.setCharacterEncoding(enc)
    def length: Int = raw.getContentLength
    def contentType: String = raw.getContentType
    def reader: BufferedReader = raw.getReader
    def stream: ServletInputStream = raw.getInputStream
    lazy val asXml: Elem = XML.load(stream)
    lazy val asString = IOUtils.toString(stream, encoding)

    // implicitly set request encoding to UTF-8
    encoding = "UTF-8"

    /*!## Multipart Requests & File Uploading {#multipart}

    Standard Servlet API doesn't provide any capabilities to work with requests of MIME type
    `multipart/form-data` which are usually used by the clients to upload files on the server.

    Circumfex API uses [Apache Commons FileUpload](http://commons.apache.org/fileupload) to
    simplify this things for you. Commons FileUpload API is very robust and can be used in a
    number of different ways, depending upon the requirements of your application.

    Commons FileUpload offers you two approaches to deal with `multipart/form-data` requests:

      * *traditional API* relies on `FileItemFactory` which can be configured to keep small files
      in memory and to store larger files on the disk, you set threshold sizes and operate with
      convenient `FileItem` objects, which could be queried for different information, like the
      name of the corresponding field, it's size in bytes, content type, etc.
      * *streaming API* does not use intermediate storage facilities and allows you to work with
      `FileItemStream` objects, which show better performance and lower memory usage.

    Circumflex Web Framework provides support for both FileUpload styles via `parseFileItems` and
    `parseFileStreams` methods respectively.

    Note, however, that you can only use one of them (and only once) while working with the request
    (it's the limitation of accessing request body in Servlet API mentioned above, so `reader`,
    `stream`, `asXml` and `asString` methods will also interfere with FileUpload).

    For more information about configuring a FileUpload environment that will suit your needs,
    visit [Commons FileUpload Project Page](http://commons.apache.org/fileupload).
    */

    /**
     * Parses multipart request into a sequence of `FileItem` (Apache Commons FileUpload)
     * using specified `factory`. Returns `Nil` if request does not have multipart content.
     */
    def parseFileItems(factory: FileItemFactory): Seq[FileItem] =
      if (multipart_?) {
        val uploader = new ServletFileUpload(factory)
        return asBuffer(uploader.parseRequest(raw).asInstanceOf[java.util.List[FileItem]])
      } else Nil

    /**
     * Parses multipart request into a sequence of `FileItem` (Apache Commons FileUpload)
     * using `DefaultFileItemFactory` with specified `sizeThreshold` and `tempStorage`. Returns
     * `Nil` if request does not have multipart content.
     */
    def parseFileItems(sizeThreshold: Int, tempStorage: File): Seq[FileItem] =
      parseFileItems(new DefaultFileItemFactory(sizeThreshold, tempStorage))

    /**
     * Parses multipart request into a sequence of `FileItem` (Apache Commons FileUpload)
     * using `DefaultFileItemFactory` with specified `sizeThreshold` and `tempStorage`. Returns
     * `Nil` if request does not have multipart content.
     */
    def parseFileItems(sizeThreshold: Int, tempStorage: String): Seq[FileItem] =
      parseFileItems(sizeThreshold, new File(tempStorage))

    /**
     * Parses multipart request into an iterator of `FileItemStream` (Apache Commons FileUpload).
     * Returns an empty iterator if request does not have multipart content.
     */
    def parseFileStreams(): Iterator[FileItemStream] = if (multipart_?) {
      val it = new ServletFileUpload().getItemIterator(raw)
      return new Iterator[FileItemStream]() {
        def next(): FileItemStream = it.next
        def hasNext: Boolean = it.hasNext
      }
    } else Iterator.empty

  }

  /*## Include & Forward

  The methods `forward` and `include` use request dispatcher mechanism provided by Servlet API
  to forward the request processing to or to include partial result from different web application
  object (such as servlet).
  */
  def forward(uri: String) = raw.getRequestDispatcher(uri).forward(raw, response.raw)
  def include(uri: String) = raw.getRequestDispatcher(uri).include(raw, response.raw)

  override def toString: String = method + " " + uri + " " + protocol

}