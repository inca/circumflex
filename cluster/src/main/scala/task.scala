package circumflex
package cluster

import core._, xml._, cache._
import java.io.File
import java.util.regex.Pattern
import org.apache.commons.io.FileUtils

/*! Server tasks are configured for execution at deploy time.
They report their output into `currentMonitor`. */
trait ServerTask extends ElemHolder {

  val _title = attr("title")
  def title = _title.getOrElse("Untitled task " + elemName)

  def server: Server

  def process()

}

/*! Task `<copy file="./path/to/dir" to="./path/to/dir">` copies
resources from `file` (relative to module root) to the location
at the server using `rsync` over `ssh`. */
class CopyTask(@transient val server: Server)
    extends ServerTask
    with StructHolder {

  def elemName = "copy"

  val _file = attr("file")
  def file = new File(server.project.baseDir, _file())

  val _to = attr("to")
  def to = new File(_to()).getPath

  def targetLocation = server.location + ":" + to

  def spawn = new ProcessMonitor {
    def key = "server.copy"
    def builder = new ProcessBuilder(
      "rsync", "-vrlptzh",
      "-e", "ssh",
      file.getCanonicalPath,
      targetLocation)
  }

  def process() {
    currentMonitor.println(
      "=== Copying " + file.getCanonicalFile + " to " + targetLocation)
    spawn.execute().join()
  }
}

/*! Task `<lessc src="path/to/less" dst="path/to/css" exclude="(.*)\.p\.less"/>`
performs compilation of LESS files. The `exclude` parameter specifies the
filename regex for exclusion files from compilation (useful with partial LESS
for inclusion). */
class LesscTask(@transient val server: Server)
    extends ServerTask
    with StructHolder {

  def elemName = "lessc"

  val _src = attr("src")
  def src = new File(server.project.baseDir,
    _src.getOrElse("src/main/webapp/public/less"))

  val _dst = attr("dst")
  def dst = new File(server.project.baseDir,
    _dst.getOrElse("src/main/webapp/public/css"))

  val _exclude = attr("exclude")
  def exclude = Pattern.compile(_exclude.getOrElse("(.*)\\.p\\.less"))

  val _force = attr("force")
  def force = parse.booleanOption(_force()).getOrElse(false)

  def process() {
    import collection.JavaConversions._
    currentMonitor.println("=== Compiling LESS files from " + src.getCanonicalPath)
    FileUtils.listFiles(src, Array("less"), true).foreach { f =>
      try {
        if (!exclude.matcher(f.getName).matches) {
          val relPath = f.getAbsolutePath
              .substring(src.getAbsolutePath.length)
              .replaceFirst("^/", "")
          val outputFile = new File(dst,
            relPath.replaceFirst("\\.less$", ".css"))
          if (!outputFile.isFile || force ||
              outputFile.lastModified < f.lastModified) {
            currentMonitor.println("Compiling " + f.getAbsolutePath)
            lessc.compile(f, outputFile, true)
          }
        }
      } catch {
        case e: Exception =>
          currentMonitor.println("Could not compile LESS file: " + f.getAbsolutePath)
      }
    }
  }

}

object lessc extends org.lesscss.LessCompiler