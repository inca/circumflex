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

class Dependency extends Artifact {

  def elemName = "dependency"

}

class ProjectProperty(val name: String)
    extends TextHolder {

  def elemName = name

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

  val _parent = new Artifact {
    def elemName = "parent"
  }

  val dependencies = new ListHolder[Artifact] {

    def elemName = "dependencies"

    def read = {
      case "dependency" => new Dependency
    }

  }

  val dependencyManagement = new StructHolder {
    def elemName = "dependencyManagement"
  }

  val modules = new ListHolder[Module] {

    def elemName = "modules"

    def read = {
      case "module" => new Module(project)
    }

    def projects = children.flatMap(_.project).map(_.load())

  }

  val properties = new ListHolder[ProjectProperty] {

    def elemName = "properties"

    def read = {
      case n => new ProjectProperty(n)
    }

  }

  val build = new StructHolder {

    def elemName = "build"

  }

  val reporting = new StructHolder {

    def elemName = "reporting"

  }

  val profiles = new StructHolder {

    def elemName = "profiles"

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
