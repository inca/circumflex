package pro.savant.circumflex
package cluster

import core._
import scala.collection.mutable.HashMap

class Job(val key: String,
          val monitor: Monitor) {

  def isEnded = !monitor.isAlive

  def outLines =
    monitor.out.map(l => "<div class=\"out\">" + wrapHtml(l) + "</div>")

  def errLines =
    monitor.err.map(l => "<div class=\"err\">" + wrapHtml(l) + "</div>")

  def title = msg.get("job." + key).getOrElse(key)

  override def toString = title

}

class ClusterStatus(val clusterId: String) {

  def cluster = getCluster(clusterId)
      .getOrElse(throw new IllegalStateException(
    "Cluster " + clusterId + " not found."))

  protected var _currentJob: Option[Job] = None

  def lastJob: Option[Job] = synchronized {
    _currentJob
  }

  def currentJob: Option[Job] = synchronized {
    _currentJob.filter(!_.isEnded)
  }

  def runJob(job: Job) {
    synchronized {
      if (currentJob.isEmpty) {
        _currentJob = Some(job)
        job.monitor.execute()
      } else throw new ValidationException("job.exists")
    }
  }

}

object status {

  private val _map = new HashMap[String, ClusterStatus]

  def get(clusterId: String): ClusterStatus =
    _map.get(clusterId) getOrElse synchronized {
      _map.get(clusterId) getOrElse {
        val s = new ClusterStatus(clusterId)
        _map += clusterId -> s
        s
      }
    }

  def get(cluster: Cluster): ClusterStatus = get(cluster.id)

}