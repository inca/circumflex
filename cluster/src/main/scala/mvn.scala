package pro.savant.circumflex
package cluster

import xml._
import java.io.File

class Project(val baseDir: File)
    extends StructHolder
    with XmlFile { project =>

  def descriptorFile = new File(baseDir, "pom.xml")

  def elemName = "project"

  val modules = new ListHolder[Module] {

    def elemName = "modules"

    def read = {
      case "module" => new Module(project)
    }

  }

}

class Module(@transient val parentProject: Project)
    extends TextHolder {

  def elemName = "module"

  lazy val project: Option[Project] = get.flatMap { name =>
    val baseDir = new File(parentProject.baseDir, name)
    if (baseDir.isDirectory)
      Some(new Project(baseDir))
    else None
  }

}
