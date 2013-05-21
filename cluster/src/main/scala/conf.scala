package circumflex
package cluster

import core._, xml._
import java.io.File

class ClusterConfiguration
    extends StructHolder
    with XmlFile {

  def elemName = "cluster"

  def descriptorFile = new File(root, "cluster.xml")

  val users = new ListHolder[User] {

    def elemName = "users"

    def read = {
      case "user" => new User
    }

  }

  val _projects = new ListHolder[ProjectConf] {

    def elemName = "projects"

    def read = {
      case "project" => new ProjectConf
    }

  }

  lazy val projects = _projects.children.flatMap(_.project)

  class ProjectConf extends StructHolder {

    def elemName = "project"

    val path = attr("path")

    lazy val project: Option[Project] = path
        .map(dir => new File(dir))
        .filter(_.isDirectory)
        .map(baseDir => new Project(baseDir))

  }

  def clusters = projects.flatMap(_.flat).map(_.cluster).filter(_.exists)

}