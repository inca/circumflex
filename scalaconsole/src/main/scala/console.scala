package pro.savant.circumflex
package scalaconsole

import core._
import tools.nsc._, interpreter._
import java.io._

class ScalaConsole(val bucketFunc: () => StringWriter) {

  val settings = new Settings()

  settings.classpath.value +=
      File.pathSeparator + System.getProperty("java.class.path") +
          File.pathSeparator + cx.getString("application.classpath").getOrElse("")
  settings.Yreplsync.value = true

  val writer = new PrintWriter(
    new Writer {
      def write(cbuf: Array[Char], off: Int, len: Int) {
        bucketFunc().write(cbuf, off, len)
      }
      def flush() {}
      def close() {}
    })

  var imain = new IMain(settings, writer)

  // Make default imports

  imain.interpret("import pro.savant._, circumflex._")
  imain.interpret("import core._, web._, orm._, xml._, cache._, diff._, mail._, markeven._")
  imain.interpret("import java.util.Date, java.io._, java.net._")
  imain.interpret("import scalaconsole.ScalaConsole._")

  def execute(cmd: String): Results.Result = try {
    ctx += "scalaconsole.out.writer" -> writer
    imain.interpret(cmd)
  } finally {
    ctx -= "scalaconsole.out.writer"
  }

  def reset() {
    imain.reset()
    imain.close()
    imain = new IMain(settings, new PrintWriter(writer))
  }

}

object ScalaConsole {

  val stdout = new PrintWriter(System.out)

  def getWriter = ctx.getAs[PrintWriter]("scalaconsole.out.writer")
      .getOrElse(stdout)

  def print(obj: Any) {
    getWriter.print(obj)
  }

  def println() {
    getWriter.println()
  }

  def println(obj: Any) {
    getWriter.println(obj)
  }

  def printf(text: String, xs: Any*) {
    getWriter.print(text.format(xs: _*))
  }

}