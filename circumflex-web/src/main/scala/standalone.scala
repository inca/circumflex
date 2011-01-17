package ru.circumflex.web

import ru.circumflex.core._
import javax.servlet.Filter
import org.mortbay.jetty.servlet.{ServletHolder, DefaultServlet, Context => JettyContext}
import org.mortbay.jetty.{Handler, Server}
import org.apache.commons.io.FilenameUtils._

/*!# Standalone Server

Circumflex Web Framework ships `StandaloneServer` which uses Jetty to
start accepting requests without even the need to use deployment descriptor
(which is otherwise required by Servlet Specification).

Following configuration parameters are required to get standalone server
up and running:

  * `cx.root` specifies content root of web application (`src/main/webapp`
  is default;
  * `cx.port` specifies port which Jetty will listen to;
  * `cx.router` specifies the class name of the main `RequestRouter`.

Usage is simple: use `start` method to bring standalone server up
and `stop` method to shut the server down.

Refer to source code and Jetty documentation for more information.
*/

/**
 * Provides standalone server based on Jetty.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0/circumflex-web/standalone.scala">standalone.scala</a>.
 */
class StandaloneServer {

  def filters: Seq[Class[_ <: Filter]] = List(classOf[CircumflexFilter])

  protected var jetty: Server = null
  protected var context: JettyContext = null

  def init() = {
    val webappRoot = cx.get("cx.root") match {
      case Some(s: String) => s
      case _ => "src/main/webapp"
    }
    jetty = new Server(cx.get("cx.port") match {
      case Some(p: Int) => p
      case Some(s: String) => try { s.toInt } catch { case _ => 8180 }
      case _ => 8180
    })
    context = new JettyContext(jetty, "/", JettyContext.SESSIONS)
    context.setResourceBase(separatorsToSystem(webappRoot))
    context.addServlet(new ServletHolder(new DefaultServlet), "/*")
    filters.foreach(f => context.addFilter(f, "/*", Handler.ALL))
  }

  def start = {
    init()
    jetty.start
  }

  def stop = if (jetty != null) jetty.stop

}
