package pro.savant.circumflex

import core._
import javax.servlet.{FilterChain, FilterConfig}
import javax.activation.MimetypesFileTypeMap
import java.io._
import org.apache.commons.io.IOUtils
import java.util.Date
import java.util.regex.Pattern
import java.net.{URLDecoder, URLEncoder}
import org.apache.commons.lang3.StringEscapeUtils

/*!# The `web` package

Package object `web` contains different shortcuts, utilities and helpers --
the basis of routing DSL of Circumflex Web Framework.

You should import this package to use Circumflex Web Framework in your application:

``` {.scala}
import pro.savant.circumflex.web._
```

If you don't wish to import all helpers into global scope, then import this
package under an alias:

``` {.scala}
import pro.savant.circumflex.{web => cx} // import under alias "cx"
cx.request                               // access members
```
*/
package object web {

  val WEB_LOG = new Logger("pro.savant.circumflex.web")

  /*!## Accessing web context

  Methods `request`, `response`, `session`, `filterChain`, `filterConfig`
  and `servletContext` can be used anywhere inside web application to
  obtain corresponding web context objects.

  Each of these methods also have their `xxxOption` counterpart which
  can be used to work in different non-web contexts.
  */
  def requestOption = ctx.getAs[HttpRequest]("cx.request")
  def request = requestOption.get

  def responseOption = ctx.getAs[HttpResponse]("cx.response")
  def response = responseOption.get

  def sessionOption = requestOption.map(_.session)
  def session = sessionOption.get

  def filterChainOption = ctx.getAs[FilterChain]("cx.filterChain")
  def filterChain = filterChainOption.get

  def filterConfigOption = cx.getAs[FilterConfig]("cx.filterConfig")
  def filterConfig = filterConfigOption.get

  def servletContextOption = filterConfigOption.map(_.getServletContext)
  def servletContext = servletContextOption.get

  /*! Additional methods allow you to access (and override via context)
  following information:

    * `scheme` as determined from the `request.scheme` (can be overridden
      be setting `cx.scheme` context variable);

    * `host` as determined from the `Host` request header (can be overridden
      by setting `cx.host` context variable);

    * `origin` -- a shortcut which combines scheme and host.
   */

  def scheme = ctx.getString("cx.scheme")
      .orElse(requestOption.map(_.scheme))
      .getOrElse("http")

  def host = ctx.getString("cx.host")
      .orElse(requestOption.map(_.serverHost))
      .getOrElse("localhost")

  def origin = scheme + "://" + host

  /*!## Sending responses

  Circumflex Web Framework provides following methods for immediately aborting
  normal control flow and sending the response from any place of your application:

    * `send` writes specified `text` to response buffer and, if specified,
      sets `statusCode`;

    * `sendError` sends an error to the client using specified `statusCode` and
      `message`;

    * `sendRedirect` sends `302 MOVED TEMPORARILY` to the client using specified
      `url` and optional `flashes`;

    * `sendFile` sends specified `file` to the client; if `filename` is provided,
      `Content-Disposition: attachment` is also added to the response with specified
      `filename`;

    * `sendJson` sends specified `json` string to the client with Content-Type
      `application/json`; designed to be used either with `json` or `jsonp` data type
      of JQuery `ajax` calls;

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

    * `pass` sends request and response down the filter chain and then immediately
      flushes response.

    * `serveLastModified` immediately sends `304 NOT MODIFIED` if two following
      conditions are met:

      * the client specified the `If-Modified-Since` header;
      * the date specified by the client is greater then specified `date` parameter.

      Otherwise, this method just sets the `Last-Modified` header and continues.

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

  def sendJsRedirect(url: String): Nothing = {
    response.contentType("application/javascript")
    send("window.location.replace(\"" + escapeJs(url) + "\");")
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

  def sendJson(json: String): Nothing  = {
    val callback = param("callback")
    if (callback != "") {
      response.contentType("application/javascript")
      send(callback + "(" + json + ")")
    } else {
      response.contentType("application/json")
      send(json)
    }
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

  def serveLastModified(lastModified: Date) {
    request.headers.getAsDate("If-Modified-Since").map { d =>
      if ((d.getTime / 1000) >= (lastModified.getTime / 1000))
        send(statusCode = 304)
    }
    response.headers += ("Last-Modified" -> lastModified)
  }

  /*!## Headers matching helpers

  The `matchers` helper contains shortcuts for various matchers
  (for example, by known HTTP headers).

  You should import this object if you want to use it:

  ``` {.scala
  import pro.savant.circumflex.web.{matchers => m}

  get("/").and(m.ACCEPT(":mime")) = "You are accepting " + param("mime")
  ```
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

  /*! ## Utilities

  A couple of utility methods are imported along with the `web` pacakge object.
  These method include `encodeURI`, `encodeURIComponent`, `decodeURI`
  and `decodeURIComponent` (like in Javascript), escaping and unescaping
  for HTML, JS and JSON input, JSON validation, etc.
  */

  // Escaping

  def escapeHtml(text: String) =
    StringEscapeUtils.escapeHtml4(text)
        .replaceAll("\'", "&#x27;")

  def unescapeHtml(text: String) =
    StringEscapeUtils.unescapeHtml4(text)
        .replaceAll("&apos;", "\'")

  def escapeJs(text: String) = StringEscapeUtils.escapeEcmaScript(text)

  def unescapeJs(text: String) = StringEscapeUtils.unescapeEcmaScript(text)

  def escapeXml(text: String) = StringEscapeUtils.escapeXml(text)

  def unescapeXml(text: String) = StringEscapeUtils.unescapeXml(text)

  // ECMA-like encode/decode

  def encodeURI(s: String) = encodeURIComponent(s)
      .replaceAll("%3B", ";")
      .replaceAll("%2F", "/")
      .replaceAll("%3F", "?")
      .replaceAll("%3A", ":")
      .replaceAll("%40", "@")
      .replaceAll("%26", "&")
      .replaceAll("%3D", "=")
      .replaceAll("%2B", "+")
      .replaceAll("%24", "$")
      .replaceAll("%2C", ",")

  def encodeURIComponent(s: String) = {
    URLEncoder.encode(s, "UTF-8")
        .replaceAll("\\+", "%20")
        .replaceAll("%21", "!")
        .replaceAll("%27", "'")
        .replaceAll("%28", "(")
        .replaceAll("%29", ")")
        .replaceAll("%7E", "~")
  }

  def decodeURI(s: String) = {
    val escaped = s.replaceAll("%(3B|2F|3F|3A|40|26|3D|2B|24|2C)", "%25$1")
    decodeURIComponent(escaped)
  }

  def decodeURIComponent(s: String) =
    URLDecoder.decode(s, "UTF-8")

  // JSON validator

  protected val jsonRegex =
    Pattern.compile("[^,:{}\\[\\]0-9.\\-+Eaeflnr-u \\n\\r\\t]")

  def validateJSON(s: String): Boolean =
    !jsonRegex.matcher(s.replaceAll("\"(\\\\.|[^\"\\\\])*\"", "")).find

  // Appending URL parameters

  def appendParam(url: String, name: String, value: String): String = {
    var result = url
    if (result.indexOf("?") == -1)
      result += "?"
    else result += "&"
    result + encodeURIComponent(name) + "=" + encodeURIComponent(value)
  }

  def appendParams(url: String, params: (String, String)*): String =
    if (params.size == 0) url
    else {
      val p = params.head
      appendParams(appendParam(url, p._1, p._2), params.tail: _*)
    }

}
