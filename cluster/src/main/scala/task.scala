package pro.savant.circumflex
package cluster

import core._, xml._, cache._
import java.io.File

/*! Server tasks are configured for execution at deploy time.
They report their output into `currentMonitor`. */
trait ServerTask extends ElemHolder {

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
    currentMonitor.println("Copying " + file.getCanonicalFile + " to " + targetLocation)
    spawn.execute().join()
  }
}

