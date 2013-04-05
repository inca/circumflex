package pro.savant.circumflex
package cluster

import java.io._
import core._, xml._, cache._
import collection.mutable.HashMap

class Cluster(val project: Project)
    extends StructHolder
    with XmlFile { cluster =>

  def elemName = "cluster"

  def baseDir = project.baseDir

  val clusterDir = new File(baseDir, "src/cluster")

  def descriptorFile = new File(clusterDir, "cluster.xml")

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

  override def toString = id

  val classesDir = new File(baseDir, "target/classes")

  val mainCxPropsFile = new File(classesDir, "cx.properties")
  val _mainCxProps = new CacheCell[PropsFile](new PropsFile(mainCxPropsFile))
  def mainCxProps = _mainCxProps.get.toMap

  val clusterCxPropsFile = new File(clusterDir, "cx.properties")
  val _clusterCxProps = new CacheCell[PropsFile](new PropsFile(clusterCxPropsFile))
  def clusterCxProps = _clusterCxProps.get.toMap

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

  /*! ### Node properties evaluation

  Each node evaluates its set of properties (which are subsequently
  written into `cx.properties` of each assembled jar) by
  reading them from following locations in specified order:

    * Project POM, including its ancestors, if any
    * `cx.properties` in `target/classes`
    * `cx.properties` in `src/cluster`
    * node properties in `src/cluster/cluster.xml` of corresponding node.

  Special property `node.address` is added above previously defined onces.
  It specifies the address of the server it works on (typically the internal
  address is used).

  Properties from each step will overwrite the onces defined earlier.
  */
  def properties: Map[String, String] = {
    val result = new HashMap[String, String]
    result ++= project.properties
    result ++= cluster.mainCxProps
    result ++= cluster.clusterCxProps
    result ++= this.toMap
    result += "node.address" -> server.address()
    result.toMap
  }

  def uuid = sha256(cluster.baseDir.getAbsolutePath + ":" + toString)

  override def toString = server.id + ":" + name()

}
