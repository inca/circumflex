package pro.savant.circumflex
package cluster

import core._
import collection.JavaConversions._
import collection.mutable.ListBuffer
import java.io.{InputStreamReader, BufferedReader}

trait Monitor extends Thread {

  protected var _output = new ListBuffer[String]
  def output = _output.toSeq

  def execute(): this.type = {
    start()
    this
  }

  def println(line: String, cssClass: String = "out") {
    _output += "<div class\"" + cssClass + "\">" + wrapHtml(line) + "</div>"
  }

  def key: String

  def title = msg.get("job." + key).getOrElse(key)

  override def toString = title

}

trait ProcessMonitor
    extends Thread
    with Monitor {

  protected val dir = builder.directory

  def builder: ProcessBuilder

  override def run() {
    setName(builder.command.mkString(" "))
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
      if (!isInterrupted) {
        println("Exit status: " + process.exitValue, "finish")
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
      println(line)
      line = out.readLine()
    }
    // STDERR
    line = err.readLine()
    while (line != null) {
      println(line, "err")
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