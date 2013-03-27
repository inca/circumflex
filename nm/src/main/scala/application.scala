package pro.savant.circumflex
package nm

import java.io.File
import core._, xml._

object Application {

  def root = new File(nm.root, "applications")

  def all: Seq[Application] = root.listFiles
      .filter(_.getName.endsWith(".xml"))
      .filter(_.isFile)
      .flatMap { f =>
    try {
      Some(new Application(f.getName.replaceAll("\\.(?i:xml)$","")).load())
    } catch {
      case e: Exception =>
        LOG.warn("File " + f.getAbsolutePath + " appears to be broken.")
        None
    }
  }

  def findByName(name: String): Option[Application] =
    all.find(_.name == name)

}

class Application(val name: String)
    extends ListHolder[Node]
    with XmlFile {

  def descriptorFile = new File(Application.root, name + ".xml")

  def elemName = "application"

  val title = attr("title")
  
  def read = {
    case "node" => new Node
  }
  
}

class Node extends StructHolder {
  
  def elemName = "node"
  
  def application = parent match {
    case Some(a: Application) => a
    case _ =>
      throw new IllegalStateException("Node must exist inside application.")
  }
  
  val title = attr("title")
  
}