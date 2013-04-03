package pro.savant.circumflex
package cluster

import xml._
import java.io.File

trait Artifact extends StructHolder {

  val _groupId = text("groupId")
  def groupId = _groupId.getOrElse("")

  val _artifactId = text("artifactId")
  def artifactId = _artifactId.getOrElse("")

  val _version = text("version")
  def version = _version.getOrElse("")

  val _packaging = text("packaging")
  def packaging = _packaging.getOrElse("jar")

}

class Project(val baseDir: File)
    extends StructHolder
    with XmlFile
    with Artifact { project =>

  def descriptorFile = new File(baseDir, "pom.xml")

  def elemName = "project"

  val _name = text("name")
  val _description = text("description")
  val _url = text("url")

  val modules = new ListHolder[Module] {

    def elemName = "modules"

    def read = {
      case "module" => new Module(project)
    }

    def projects = children.flatMap(_.project).map(_.load())

  }

  def subprojects: Seq[Project] =
    modules.projects.flatMap(p => Seq(p) ++ p.subprojects)

  def getArtifactPath = groupId.replaceAll("\\.", "/") + "/" +
      version + "/" +
      artifactId + "-" + version + "." + packaging

  override def toString = groupId + ":" + artifactId + ":" + version

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
