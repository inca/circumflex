package pro.savant.circumflex
package cluster

import core._
import scala.collection.mutable.HashMap

class ClusterStatus(val clusterId: String) {

  def cluster = getCluster(clusterId)
      .getOrElse(throw new IllegalStateException(
    "Cluster " + clusterId + " not found."))

  protected var _currentJob: Option[Monitor] = None

  def lastJob: Option[Monitor] = synchronized {
    _currentJob
  }

  def currentJob: Option[Monitor] = synchronized {
    _currentJob.filter(_.isAlive)
  }

  def runJob(job: Monitor) {
    synchronized {
      if (currentJob.isEmpty) {
        _currentJob = Some(job)
        job.execute()
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