package ru.circumflex.core

import javax.servlet.http.HttpServletResponse
import org.apache.commons.io.IOUtils
import javax.activation.MimetypesFileTypeMap
import java.io._
import javax.servlet.ServletOutputStream
import collection.mutable.{ListBuffer, HashMap}
import java.lang.String

/*!# HTTP Response

The `HttpResponse` class provides functionality to prepare HTTP responses which will be
sent to clients.

This class is designed to hold the response state, which then will be applied to
actual `HttpServletResponse` using the `apply` method.

Since Circumflex is UTF-friendly it will implicitly set character encoding of
response body to `UTF-8`. Feel free to change it if your application requires so.
*/

/**
 * Provides functionality for sending HTTP responses.
 *
 * Most methods are self-descriptive and have direct equivalents in Servlet API, so
 * Scaladocs are omitted.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0/circumflex-web/response.scala">response.scala</a>.
 */
class HttpResponse {

  def apply(response: HttpServletResponse) = {
    if (bufferSize > -1)
      response.setBufferSize(bufferSize)
    response.setContentType(contentType)
    response.setCharacterEncoding(encoding)
    response.setStatus(statusCode)
    if (contentLength > -1)
      response.setContentLength(contentLength)
    // apply headers
    headers.foreach((k,v) => response.setHeader(k, v))
    // apply cookies
    cookies.foreach(c => response.addCookie(c.convert))
    // write response body
    body(response)
    // flush
    response.flushBuffer
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
  var bufferSize: Int = -1      // -1 means that container's default will be used
  var contentType: String = "text/html"
  var encoding: String = "UTF-8"
  var statusCode: Int = 200
  var contentLength: Int = -1   // -1 means that container's default will be used

  /*!## Response Body

  The body of the response is set by supplying a function which works with `HttpServletResponse`
  passed inside that function.
  The function is invoked inside the `apply` method when response is completely ready
  to be sent -- this is done to avoid `IllegalStateException`s when working with response.
  */
  var body: (HttpServletResponse) => Unit = r => {}

  /*!## Headers

  Response headers contain operational information about the response.
  Circumflex Web Framework lets you access response headers via the `headers` object.
  */
  val headers = HashMap[String, String]("X-Powered-By" -> "Circumflex 2.0")

  /*!## Cookies

  The `cookies` field lets you set response cookies.
  */
  val cookies = new ListBuffer[HttpCookie]

}

/* TODO implement following response helpers:

  * empty
  * error
  * redirect
  * text
  * xml
  * binary (write byte array)
  * file
  * stream
  * writer

 */
