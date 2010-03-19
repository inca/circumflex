package ru.circumflex.core
import java.io.File
import javax.servlet.Filter
import org.mortbay.jetty.servlet.{ServletHolder, DefaultServlet, Context}
import org.mortbay.jetty.{Handler, Server}
import org.apache.commons.io.FilenameUtils._

/**
 * A helper that allows standalone Circumflex execution based on
 * Jetty server.
 */
class StandaloneServer {

  def filters: Seq[Class[_ <: Filter]] = List(classOf[CircumflexFilter])

  protected var jetty: Server = null
  protected var context: Context = null

  def init() = {
    val webappRoot = Circumflex.cfg("cx.root") match {
      case Some(s: String) => s
      case _ => "src/main/webapp"
    }
    jetty = new Server(Circumflex.cfg("cx.port") match {
      case Some(p: Int) => p
      case Some(s: String) => try { s.toInt } catch { case _ => 8180 }
      case _ => 8180
    })
    context = new Context(jetty, "/", Context.SESSIONS)
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
