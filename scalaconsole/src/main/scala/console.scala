package circumflex
package scalaconsole

import core._, web._
import tools.nsc._, interpreter._
import java.io._

class StringBucket extends StringWriter with Serializable

class ScalaConsole(val bucketFunc: () => StringBucket)
    extends Serializable {

  val settings = new Settings()

  settings.classpath.value +=
      File.pathSeparator + System.getProperty("java.class.path") +
          File.pathSeparator + cx.getString("application.classpath").getOrElse("")
  settings.Yreplsync.value = true

  protected var _out = new ByteArrayOutputStream(4096)

  def flushOutput(): String = {
    val result = new String(_out.toByteArray, "UTF-8")
    _out.close()
    _out = new ByteArrayOutputStream(4096)
    result
  }

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

  imain.interpret("import circumflex._")
  imain.interpret("import core._, web._, orm._, xml._, cache._, diff._, mail._, markeven._")
  imain.interpret("import java.util.Date, java.io._, java.net._")

  def execute(cmd: String): Results.Result =
    scala.Console.withOut(_out) {
      imain.interpret(cmd)
    }

  def reset() {
    imain.reset()
    imain.close()
    imain = new IMain(settings, new PrintWriter(writer))
  }

}
