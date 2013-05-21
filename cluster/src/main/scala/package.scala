package circumflex

import core._, cache._
import java.io.File
import java.util.regex.Pattern

package object cluster {

  val CL_LOG = new Logger("circumflex.cluster")

  val DEFAULT_FILTER_PATTERN = Pattern.compile("^.*\\.(?:xml|properties)$")
  val FILTER_TOKEN_PATTERN = Pattern.compile("\\$\\{(.*?)\\}")

  val root = cx.getString("cx.cluster.root") match {
    case Some(path) => new File(path)
    case _ => new File(System.getProperty("user.home"), ".cx")
  }

  val domain = cx.getString("cx.cluster.domain").getOrElse("localhost:33300")

  val _conf = new CacheCell[ClusterConfiguration](
    new ClusterConfiguration().load())

  def conf = _conf.get

  def getProject(name: String) = conf.projects.find(_.name == name)

  def project(name: String) = getProject(name).get

  def getCluster(id: String) = conf.clusters.find(_.id == id)

  def cluster(id: String) = getCluster(id).get

  def getCurrentMonitor = ctx.getAs[Monitor]("monitor")

  def currentMonitor = getCurrentMonitor.getOrElse(EmptyMonitor)

}