package pro.savant.circumflex
package cluster

import java.io._
import core._, xml._

class Cluster(val project: Project)
    extends StructHolder
    with XmlFile { cluster =>

  def descriptorFile = new File(project.baseDir, "src/cluster/cluster.xml")

  def elemName = "cluster"

  val _id = attr("id")
  def id = _id.getOrElse("")

  val servers = new ListHolder[Server] {

    def elemName = "servers"

    def read = {
      case "server" => new Server(cluster)
    }

  }

  val mainNodes = new ListHolder[Node] {

    def elemName = "main-nodes"

    def read = {
      case "node" => new Node(cluster)
    }

  }

  val backupNodes = new ListHolder[Node] {

    def elemName = "backup-nodes"

    def read = {
      case "node" => new Node(cluster)
    }

  }

  override def toString = id

  // Load on instantiate
  load()

}

class Server(val cluster: Cluster)
    extends StructHolder {

  def elemName = "server"

  def project = cluster.project

  val address = attr("address")

  val user = attr("user")

  val dir = attr("dir")

  override def toString = user() + "@" + address() + ":" + dir()

}

class Node(val cluster: Cluster)
    extends PropsHolder("node") {

  def project = cluster.project

  val name = attr("name")

  def properties = project.properties ++ toMap

  override def toString = cluster.toString + ":" + name()

}
