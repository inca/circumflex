package circumflex
package web

import core._
import org.eclipse.jetty.webapp.WebAppContext
import org.eclipse.jetty.server.{Handler, Server}
import java.net.{InetAddress, InetSocketAddress}

/*!# Standalone Server

Circumflex Web Framework ships `StandaloneServer` which uses Jetty
under the curtains.

It is primarily used for testing by `MockApp` object.

Following configuration parameters are required to get standalone server
up and running:

  * `cx.webappRoot` specifies content root of web application (`src/main/webapp`
  is default);
  * `cx.contextPath` specifies the context path of web application (`/` by default);
  * `cx.port` specifies port which Jetty will listen to.

Jetty's `WebAppContext` is used as default requests handler: it provides sensible defaults
for almost every web application startup (it reads configuration from `web.xml` and all other
Jetty-specific descriptors). You can override the server with your own implementation using
the `cx.jetty.server` configuration property.

Refer to Jetty documentation for more information.
*/
class StandaloneServer {

  val listenAddress: InetAddress = cx.get("cx.listenAddress") match {
    case Some(a: InetAddress) => a
    case Some(s: String) => InetAddress.getByName(s)
    case _ => InetAddress.getByName("0.0.0.0")
  }

  val port: Int = cx.get("cx.port") match {
    case Some(p: Int) => p
    case Some(s: String) => try { s.toInt } catch { case e: Exception => 8180 }
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

  lazy val server = cx.instantiate[Server](
    "cx.jetty.server", prepareDefaultServer)

  protected def prepareDefaultServer: Server = {
    val srv = new Server(new InetSocketAddress(listenAddress, port))
    srv.setHandler(handler)
    srv
  }

  lazy val handler = cx.instantiate[Handler](
    "cx.jetty.handler", prepareDefaultHandler)

  protected def prepareDefaultHandler: Handler =
    new WebAppContext(webappRoot, contextPath)

  def start() {
    server.start()
  }

  def stop() {
    server.stop()
  }
}
