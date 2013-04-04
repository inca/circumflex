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

  def getServer(id: String) = servers.children.find(_.id == id)

  def server(id: String) = getServer(id).get

  def getNode(id: String) = servers
      .children
      .find(s => id.startsWith(s.id + ":"))
      .flatMap(s => s.children.find(_.toString == id))

  def node(id: String) = getNode(id).get

  def baseDir = project.baseDir

  override def toString = id

  // Load on instantiate
  load()

}

class Server(val cluster: Cluster)
    extends ListHolder[Node] { server =>

  def elemName = "server"

  def project = cluster.project

  val _id = attr("id")
  def id = _id.get.orElse(address.get).getOrElse("")

  val address = attr("address")

  val user = attr("user")

  val dir = attr("dir")

  def read = {
    case "node" => new Node(server)
  }

  def getNode(name: String) = children.find(_.name == name)

  def node(name: String) = getNode(name).get

  override def toString = user() + "@" + address() + ":" + dir()

}

class Node(val server: Server)
    extends PropsHolder("node") {

  def cluster = server.cluster

  def project = cluster.project

  val name = attr("name")

  val backup = attr("backup")

  def isBackup = backup.getOrElse("false") == "true"

  def properties = project.properties ++
      toMap ++
      Seq("node.address" -> server.address)

  def uuid = sha256(cluster.baseDir.getAbsolutePath + ":" + toString)

  override def toString = server.id + ":" + name()

}
