package ru.circumflex.web

import ru.circumflex.core._
import javax.servlet.Filter
import org.mortbay.jetty.servlet.{ServletHolder, DefaultServlet, Context => JettyContext}
import org.mortbay.jetty.{Handler, Server}
import org.apache.commons.io.FilenameUtils._

// TODO add documentation for standalone server
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
