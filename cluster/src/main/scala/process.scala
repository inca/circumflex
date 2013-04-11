package pro.savant.circumflex
package cluster

import core._
import collection.JavaConversions._
import org.apache.commons.io.IOUtils

class ProcessMonitor(val builder: ProcessBuilder)
    extends Thread(builder.command.mkString(" ")) {

  protected val dir = builder.directory

  def execute(): this.type = {
    start()
    this
  }

  override def run() {
    val process = builder.start()
    val result = time {
      try {
        // Closing STDIN
        process.getOutputStream.close()
        // We read output from both streams repeatedly until the process exits
        while (isAlive(process) && !isInterrupted)
          try {
            readStreams(process)
            Thread.sleep(1000)
          } catch {
            case e: InterruptedException =>
              interrupt()
          }
      } finally {
        process.getInputStream.close()
        process.getErrorStream.close()
      }
    }
    if (!isInterrupted) {
      val execTime = if (result._1 > 1000)
        result._1 / 1000 + " s"
      else result._1 + " ms"
      CL_LOG.info("Exited with status " + process.exitValue +
          ", execution time " + execTime)
    }
  }

  protected def readStreams(process: Process) {
    // STDOUT
    val out = IOUtils.toString(process.getInputStream, "UTF-8")
    if (out != "") {
      CL_LOG.info(out)
    }
    // STDERR
    val err = IOUtils.toString(process.getErrorStream, "UTF-8")
    if (err != "") {
      CL_LOG.error(err)
    }
  }

  protected def isAlive(process: Process) = try {
    process.exitValue
    false
  } catch {
    case e: Exception => true
  }

}