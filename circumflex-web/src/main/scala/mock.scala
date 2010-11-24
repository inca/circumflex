package ru.circumflex.web

import ru.circumflex.core._
import javax.servlet.http.Cookie
import org.mortbay.jetty.Handler
import org.mortbay.jetty.servlet.{DefaultServlet}
import org.mortbay.jetty.testing.{HttpTester, ServletTester}

/*!# Testing your application

Circumflex Web Framework lets you test your web application using the `MockApp`.

Refer to our test sources at [`circumflex-web/src/test/scala`][tests] to see it in action.

   [tests]: http://github.com/inca/circumflex/tree/master/circumflex-web/src/test/scala/
*/

/**
 * Provides functionality to test your web application.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0.RC2/circumflex-web/mock.scala">mock.scala</a>.
 */
trait MockServer extends StandaloneServer {

  protected var _tester: ServletTester = null

  def tester = _tester

  def initTester() = {
    _tester = new ServletTester()
    _tester.setContextPath("/")
    _tester.setResourceBase(Circumflex.get("cx.root") match {
      case Some(s: String) => s
      case _ => "src/main/webapp"
    })
    _tester.addServlet(classOf[DefaultServlet], "/*")
    filters.foreach(f => _tester.addFilter(f, "/*", Handler.ALL))
  }

  override def start = {
    initTester()
    _tester.start
  }

  override def stop = if (_tester != null) _tester.stop

  // ## HTTP Methods

  def get(uri: String) = new MockRequest(this, "GET", uri)
  def head(uri: String) = new MockRequest(this, "HEAD", uri)
  def post(uri: String) = new MockRequest(this, "POST", uri)
          .setHeader("Content-Type", "application/x-www-form-urlencoded")
  def put(uri: String) = new MockRequest(this, "PUT", uri)
          .setHeader("Content-Type", "application/x-www-form-urlencoded")
  def delete(uri: String) = new MockRequest(this, "DELETE", uri)
          .setHeader("Content-Type", "application/x-www-form-urlencoded")
  def options(uri: String) = new MockRequest(this, "OPTIONS", uri)
          .setHeader("Content-Type", "application/x-www-form-urlencoded")
  def patch(uri: String) = new MockRequest(this, "PATCH", uri)
          .setHeader("Content-Type", "application/x-www-form-urlencoded")

}

/**
 * @see MockServer
 */
class MockRequest(val mockServer: MockServer, val method: String, val uri: String) {

  private val req = new HttpTester

  req.setMethod(method)
  req.setURI(uri)
  req.setVersion("HTTP/1.1")
  req.setHeader("Host", "localhost")

  def setHeader(name: String, value: String): this.type = {
    req.setHeader(name, value)
    return this
  }

  def setDateHeader(name: String, value: Long): this.type = {
    req.setDateHeader(name, value)
    return this
  }

  def setLongHeader(name: String, value: Long): this.type = {
    req.setLongHeader(name, value)
    return this
  }

  def setContent(content: String): this.type = {
    req.setContent(content)
    return this
  }

  def setCookie(cookie: Cookie): this.type = {
    req.addSetCookie(cookie)
    return this
  }

  def get_? = req.getMethod.equalsIgnoreCase("GET")
  def post_? = req.getMethod.equalsIgnoreCase("POST")
  def put_? = req.getMethod.equalsIgnoreCase("PUT")
  def delete_? = req.getMethod.equalsIgnoreCase("DELETE")
  def head_? = req.getMethod.equalsIgnoreCase("HEAD")
  def options_? = req.getMethod.equalsIgnoreCase("OPTIONS")
  def patch_? = req.getMethod.equalsIgnoreCase("PATCH")

  override def toString = req.generate

  def execute(): HttpTester = {
    val result = new HttpTester
    result.parse(mockServer.tester.getResponses(req.generate))
    return result
  }

}

/**
 * @see MockServer
 */
object MockApp extends MockServer
