package pro.savant.circumflex
package cluster

import collection.JavaConversions._
import java.io.{InputStreamReader, BufferedReader}
import java.util.concurrent.LinkedBlockingQueue

class ProcessMonitor(val builder: ProcessBuilder)
    extends Thread(builder.command.mkString(" ")) {

  protected val dir = builder.directory

  protected var _out: String = ""
  def out = _out

  protected var _err: String = ""
  def err = _err

  val outQueue = new LinkedBlockingQueue[String]
  val errQueue = new LinkedBlockingQueue[String]

  def execute(): this.type = {
    start()
    this
  }

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
      _out += line + "\n"
      outQueue.put(line)
      line = out.readLine()
    }
    // STDERR
    line = err.readLine()
    while (line != null) {
      _err += line + "\n"
      errQueue.put(line)
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