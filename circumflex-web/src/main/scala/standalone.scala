package ru.circumflex.web

import ru.circumflex.core._
import org.mortbay.jetty.servlet.{Context => JettyContext}
import org.mortbay.jetty.webapp.WebAppContext
import org.mortbay.jetty.{Server}

/*!# Standalone Server

Circumflex Web Framework ships `StandaloneServer` which uses Jetty under the curtains.
It can be used to start server manually (e.g. via Scala console). Usage is simple: use
`start` method to bring standalone server up and `stop` method to shut the server down.
Initialization are not synchronized, so if you intend dynamic start-stop functionality,
consider external synchronization.

The best practice for setting up the standalone server is to provide a singleton object:

    object MyServer extends StandaloneServer

Following configuration parameters are required to get standalone server
up and running:

  * `cx.webappRoot` specifies content root of web application (`src/main/webapp`
  is default);
  * `cx.contextPath` specifies the context path of web application (`/` by default);
  * `cx.port` specifies port which Jetty will listen to.

Jetty's `WebAppContext` is used as default requests handler: it provides sensible defaults
for almost every web application startup (it reads configuration from `web.xml` and all other
Jetty-specific descriptors). You can override default handler:

    object MyServer extends StandaloneServer {
      context(new JettyContext(...))
    }

You can also supply your own implementation of Jetty's `Server`:

    object MyServer extends StandaloneServer {
       server(new JettyServer(...))
    }

Refer to Jetty documentation for more information.
*/

/**
 * Provides standalone server based on Jetty.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0.2/circumflex-web/standalone.scala">standalone.scala</a>.
 */
class StandaloneServer {

  protected var _jetty: Server = null
  protected var _context: JettyContext = null

  val port: Int = cx.get("cx.port") match {
    case Some(p: Int) => p
    case Some(s: String) => try { s.toInt } catch { case _ => 8180 }
    case _ => 8180
  }

  val webappRoot: String = cx.get("cx.webappRoot") match {
    case Some(s: String) => s
    case _ => "src/main/webapp"
  }

  val contextPath: String = cx.get("cx.contextPath") match {
    case Some(s: String) => s
    case _ => "/"
  }

  def context(jettyContext: JettyContext): this.type = {
    _context = jettyContext
    return this
  }

  def context: JettyContext = {
    if (_context == null) {
      _context = new WebAppContext(webappRoot, contextPath)
    }
    return _context
  }

  def server(jettyServer: Server): this.type = {
    _jetty = jettyServer
    return this
  }

  def server: Server = {
    if (_jetty == null) {
      _jetty = new Server(port)
      _jetty.setHandler(context)
    }
    return _jetty
  }

  def start = server.start
  def stop = if (_jetty != null) _jetty.stop

}
