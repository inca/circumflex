package pro.savant.circumflex
package cluster

import xml._, cache._
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

class Project(val baseDir: File,
              val parentModule: Option[Module] = None)
    extends StructHolder
    with XmlFile
    with Artifact { project =>

  def descriptorFile = new File(baseDir, "pom.xml")

  def elemName = "project"

  def name = baseDir.getName

  val _properties = new PropsHolder("properties", baseDir)

  def properties: Map[String, String] =
    parentProject.map(_.properties).getOrElse(Map()) ++
        _properties.toMap

  val modules = new ListHolder[Module] {

    def elemName = "modules"

    def read = {
      case "module" => new Module(project)
    }

    lazy val projects = children.flatMap(_.project)

  }

  def getModule(name: String) = modules.projects.find(_.name == name)

  def module(name: String) = getModule(name).get

  def parentProject: Option[Project] = parentModule.map(_.parentProject)

  def rootProject: Project = parentProject.map(_.rootProject).getOrElse(this)

  def subprojects: Seq[Project] = modules.projects.flatMap(_.flat)

  def flat: Seq[Project] = Seq(this) ++ subprojects

  def lastCommitLog = {
    import scala.sys.process._
    try {
      val pb = new java.lang.ProcessBuilder("git", "log", "-n", "1")
      pb.directory(baseDir)
      pb.!!
    } catch {
      case e: Exception => "No git repository initialized."
    }
  }

  val _cluster = new CacheCell[Cluster](new Cluster(project))
  def cluster = _cluster.get

  def getArtifactPath = groupId.replaceAll("\\.", "/") + "/" +
      version + "/" +
      artifactId + "-" + version + "." + packaging

  override def toString = groupId + ":" + artifactId + ":" + version

  def exec(args: String*): ProcessBuilder = {
    val b = new ProcessBuilder(args: _*)
    b.directory(baseDir)
    b
  }

  def mvn(args: String*) = exec((Seq("mvn") ++ args): _*)

  // Load on instantiate
  load()

}

class Module(@transient val parentProject: Project)
    extends TextHolder {

  def elemName = "module"

  lazy val project: Option[Project] = get
      .map(dir => new File(parentProject.baseDir, dir))
      .filter(_.isDirectory)
      .map(baseDir => new Project(baseDir, Some(this)))

}