package ru.circumflex.web

import javax.servlet.http.HttpServletResponse
import org.apache.commons.io.IOUtils
import javax.activation.MimetypesFileTypeMap
import java.io._
import javax.servlet.ServletOutputStream
import collection.mutable.{ListBuffer, HashMap}
import java.lang.String
import java.util.Date

/*!# HTTP Response

The `HttpResponse` class provides functionality to prepare HTTP responses which will be
sent to clients.

This class is designed to hold the response state, which then will be applied to
actual `HttpServletResponse` using the `flush` method.

Since Circumflex is UTF-friendly it will implicitly set character encoding of
response body to `UTF-8`. Feel free to change it if your application requires so.
*/
class HttpResponse(val raw: HttpServletResponse) {

  def flush(): Nothing = if (!raw.isCommitted) {
    if (statusCode != -1)
      raw.setStatus(statusCode)
    if (contentLength != -1)
      raw.setContentLength(contentLength)
    // apply headers
    headers.foreach {
      case (k: String, v: Date) => raw.setDateHeader(k, v.getTime)
      case (k: String, v: Int) => raw.setIntHeader(k, v)
      case (k: String, v) => raw.setHeader(k, v.toString)
    }
    // apply cookies
    cookies.foreach(c => raw.addCookie(c.convert))
    // write response body
    body(raw)
    // flush
    raw.flushBuffer()
    // throw an exception to container
    throw new ResponseSentException()
  }

  /*!## Response Basics

    * `bufferSize` returns or sets the size of response buffer;
    * `contentType` returns or sets the MIME type of the response, the default one is
    `text/html` (we are web framework after all!);
    * `encoding` returns or sets the name of the character encoding used in response body,
    as mentioned above, we implicitly set this to `UTF-8`;
    * `statusCode` returns or sets the status code of the response.
    * `contentLength` returns or sets the `Content-Length` header of the response.
  */
  def bufferSize = raw.getBufferSize
  def bufferSize(bs: Int): this.type = {
    raw.setBufferSize(bs)
    return this
  }
  def contentType = raw.getContentType
  def contentType(ct: String): this.type = {
    raw.setContentType(ct)
    return this
  }
  def encoding = raw.getCharacterEncoding
  def encoding(e: String): this.type = {
    raw.setCharacterEncoding(e)
    return this
  }
  protected var _statusCode: Int = -1
  def statusCode = _statusCode
  def statusCode(sc: Int): this.type = {
    _statusCode = sc
    return this
  }
  protected var _contentLength: Int = -1   // -1 means that container's default will be used
  def contentLength = _contentLength
  def contentLength(cl: Int): this.type = {
    _contentLength = cl
    return this
  }

  // set encoding implicitly
  encoding("UTF-8")

  /*!## Response Body

  The body of the response is set by supplying a function which works with `HttpServletResponse`
  passed inside that function.
  The function is invoked inside the `flush` method when response is completely ready
  to be sent -- this is done to avoid `IllegalStateException`s when working with response.
  */
  protected var _body: HttpServletResponse => Unit = r => {}
  def body = _body
  def body(f: HttpServletResponse => Unit): this.type = {
    _body = f
    return this
  }

  /*!## Headers

  Response headers contain operational information about the response.
  Circumflex Web Framework lets you access response headers via the `headers` object.
  */
  object headers extends HashMap[String, Any] {
    update("X-Powered-By", "Circumflex 2.1")
  }

  /*!## Cookies

  The `cookies` field lets you set response cookies.
  */
  val cookies = new ListBuffer[HttpCookie]

  /*!## Helpers

  Circumflex Web Framework includes following helpers to perform some common tasks:

    * set `Content-Disposition` header to `attachment` with specified `filename`
    using the `attachment` method;
    * set `Pragma: no-cache`, `Cache-Control: no-store` and `Expires: 0` to disable
    client-side cache using `noCache` method.
  */
  def attachment(filename: String): this.type = {
    headers("Content-Disposition") =
        "attachment; filename=\"" + new String(filename.getBytes("UTF-8"), "ISO-8859-1") + "\""
    return this
  }

  def noCache(): this.type = {
    headers("Pragma") = "no-cache"
    headers("Cache-Control") = "no-store"
    headers("Expires") = 0l
    return this
  }

}
