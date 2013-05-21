package pro.savant.circumflex
package scalaconsole

import core._, web._, freemarker._
import java.io.StringWriter
import _root_.freemarker.template.Configuration

class ScalaConsoleRouter(_templatesRoot: String,
                         val ftlConf: Configuration)
    extends Router {

  val templatesRoot =
    if (_templatesRoot.endsWith("/")) _templatesRoot
    else _templatesRoot + "/"

  get("/?") = {
    ctx.update("ftl.configuration", ftlConf)
    ftl(templatesRoot + "index.ftl")
  }

  post("/?").and(request.isXHR) = {
    val cmd = param("cmd").trim
    var result = "<div class=\"input\">" + wrapHtml(cmd) + "</div>"
    val console = getConsole
    dropBucket()
    // Execute command
    import scala.tools.nsc._
    console.execute(cmd) match {
      case interpreter.Results.Error =>
        result += "<div class=\"error\">ERROR " +
            wrapHtml(getBucket.toString) +
            "</div>"
        dropBucket()
      case interpreter.Results.Success =>
        result += "<div class=\"success\">" +
            wrapHtml(getBucket.toString) +
            "</div>"
        dropBucket()
      case _ =>
    }
    // Also flush output, if any
    val output = console.flushOutput()
    if (output != "") {
      val lines = output.split("\\n").map(_.trim)
          .map(line => "<div class=\"line\">" + line + "</div>")
      result += "<div class=\"output\">" + lines.mkString + "</div>"
    }
    send(result)
  }

  get("/reset") = {
    ctx.update("ftl.configuration", ftlConf)
    ftl(templatesRoot + "reset.ftl")
  }

  post("/reset") = {
    getConsole.imain.reset()
    getConsole.imain.close()
    session -= "scala.console"
    sendRedirect(prefix + "/")
  }

  def getConsole: ScalaConsole = session.get("scala.console") match {
    case Some(c: ScalaConsole) => c
    case _ =>
      val c = new ScalaConsole(() => getBucket)
      session += "scala.console" -> c
      c
  }

  def getBucket: StringBucket = session.get("scala.console.bucket") match {
    case Some(w: StringBucket) => w
    case _ =>
      val w = new StringBucket
      session += "scala.console.bucket" -> w
      w
  }

  def dropBucket() {
    session -= "scala.console.bucket"
  }

}

class DefaultScalaConsoleRouter
  extends ScalaConsoleRouter(
    "/scalaconsole", new DefaultConfiguration)