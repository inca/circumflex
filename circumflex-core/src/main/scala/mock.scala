package ru.circumflex.core.test

import javax.servlet.http.Cookie
import org.mortbay.jetty.Handler
import org.mortbay.jetty.servlet.{DefaultServlet}
import org.mortbay.jetty.testing.{HttpTester, ServletTester}
import ru.circumflex.core._

trait MockServer extends StandaloneServer {

  protected var _tester: ServletTester = null

  def tester = _tester

  def initTester() = {
    _tester = new ServletTester()
    _tester.setContextPath("/")
    _tester.setResourceBase(Circumflex.cfg("cx.root") match {
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

  /* ## HTTP Methods */

  def get(uri: String) = new MockRequest(this, "GET", uri)

  def post(uri: String) = new MockRequest(this, "POST", uri)

  def put(uri: String) = new MockRequest(this, "PUT", uri)

  def delete(uri: String) = new MockRequest(this, "DELETE", uri)

  def head(uri: String) = new MockRequest(this, "HEAD", uri)

  def options(uri: String) = new MockRequest(this, "OPTIONS", uri)

}

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

  override def toString = req.generate

  def execute(): HttpTester = {
    val result = new HttpTester
    if (post_?) req.setHeader("Content-Type", "application/x-www-form-urlencoded")
    result.parse(mockServer.tester.getResponses(req.generate))
    return result
  }

}

object MockApp extends MockServer
