package pro.savant.circumflex
package core

import java.util.concurrent.LinkedBlockingQueue
import collection.mutable.ListBuffer

/*!# Tasks API

Task managers provide quick-and-dirty way to execute arbitrary code
in background.`TaskManager` is essentially a blocking queue
of parameterless functions, which are executed in a separate
(background) thread.

Unlike futures, task managers provide no way to join the thread
and wait for the execution. There is also no way to obtain result,
nor process the exceptions (they are just logged into `CX_LOG`).

*/
trait TaskManager {
  protected val _queue = new LinkedBlockingQueue[() => Unit]

  def enqueue(op: () => Unit) {
    _queue.add(op)
  }

  def threadName = getClass.getSimpleName

  protected var _worker: Thread = spawnWorker

  def spawnWorker = new Thread(threadName) {
    override def run() {
      while (!isInterrupted) try {
        val op = _queue.take()
        try {
          op()
        } catch {
          case e: Exception =>
            CX_LOG.error("Failed to process task.", e)
        }
      } catch {
        case e: InterruptedException =>
          interrupt()
      }
    }
  }

  def start() {
    if (!_worker.isAlive) {
      try {
        _worker.start()
      } catch {
        case e: IllegalThreadStateException => // thread is dead
          _worker = spawnWorker
          _worker.start()
      }
      CX_LOG.info("Started task manager " + getClass.getSimpleName)
    }
  }

  def stop() {
    _worker.interrupt()
    CX_LOG.info("Stopped task manager " + getClass.getSimpleName)
  }

  // Auto-start

  start()

}

trait ContextTaskManager extends TaskManager {
  val uuid = randomUUID
  val KEY = "task.mgr." + uuid

  protected def getOps: ListBuffer[() => Unit] =
    ctx.getAs[ListBuffer[() => Unit]](KEY).getOrElse {
      val list = new ListBuffer[() => Unit]
      ctx.update(KEY, list)
      list
    }

  override def enqueue(op: () => Unit) {
    initCtxFinalizer()
    getOps += op
  }

  protected def processOps(ops: Seq[() => Unit]) {
    CX_LOG.trace("Executing " + ops.size + " operations.")
    ops.foreach { o =>
      try {
        o.apply()
      } catch {
        case e: Exception =>
          CX_LOG.debug("Failed to process operation.", e)
      }
    }
  }

  protected def initCtxFinalizer() {
    ctx.enqueueFinalizer(KEY, { () =>
      val ops = getOps
      if (!ops.isEmpty)
        _queue.add({ () =>
          Context.executeInNew { ctx =>
            try {
              processOps(ops)
            } catch {
              case e: Exception =>
                CX_LOG.debug("Failed to process context operations.", e)
            }
          }
        })
    })
  }

}
