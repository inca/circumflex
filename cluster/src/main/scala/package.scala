package pro.savant.circumflex

import core._, cache._
import java.io.File

package object cluster {

  val LOG = new Logger("pro.savant.circumflex.cluster")

  val root = cx.getString("cx.cluster.root") match {
    case Some(path) => new File(path)
    case _ => new File(System.getProperty("user.home"), ".cx")
  }

  val domain = cx.getString("cx.cluster.domain").getOrElse("localhost")

  val _conf = new CacheCell[ClusterConfiguration](
    new ClusterConfiguration().load())

  def conf = _conf.get

  def getProject(name: String) = conf.projects.find(_.name == name)

  def project(name: String) = getProject(name).get

}