package pro.savant.circumflex
package cluster

import collection.JavaConversions._
import collection.mutable.ListBuffer
import java.io.{InputStreamReader, BufferedReader}

trait Monitor extends Thread {

  def out: Seq[String]

  def err: Seq[String]

  def execute(): this.type = {
    start()
    this
  }

}

class ProcessMonitor(val builder: ProcessBuilder)
    extends Thread(builder.command.mkString(" "))
    with Monitor {

  protected val dir = builder.directory

  protected var _out = new ListBuffer[String]
  def out = _out.toSeq

  protected var _err = new ListBuffer[String]
  def err = _err.toSeq

  override def run() {
    val process = builder.start()
    // Prepare readers
    val stdout = new BufferedReader(
      new InputStreamReader(
        process.getInputStream, "UTF-8"))
    val stderr = new BufferedReader(
      new InputStreamReader(
        process.getErrorStream, "UTF-8"))
    // Closing STDIN
    process.getOutputStream.close()
    try {
      // We read output from both streams repeatedly until the process exits
      while (isAlive(process) && !isInterrupted)
        try {
          readStreams(stdout, stderr)
          Thread.sleep(1000)
        } catch {
          case e: InterruptedException =>
            interrupt()
        }
    } finally {
      process.getInputStream.close()
      process.getErrorStream.close()
      stdout.close()
      stderr.close()
    }
  }

  protected def readStreams(out: BufferedReader, err: BufferedReader) {
    // STDOUT
    var line = out.readLine()
    while (line != null) {
      _out += line
      line = out.readLine()
    }
    // STDERR
    line = err.readLine()
    while (line != null) {
      _err += line
      line = err.readLine()
    }
  }

  protected def isAlive(process: Process) = try {
    process.exitValue
    false
  } catch {
    case e: Exception => true
  }

}