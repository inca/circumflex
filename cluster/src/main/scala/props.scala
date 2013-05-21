package circumflex
package cluster

import core._, xml._, cache._
import java.io._

class Prop(val name: String) extends TextHolder {

  def elemName = name

  def baseDir = parent match {
    case Some(h: PropsHolder) => h.baseDir
    case _ => new File(".")
  }

  def toMap: Map[String, String] = {
    val value = getOrElse("")
    val file = new File(baseDir, value)
    if (name.startsWith("include.") && file.isFile)
      new PropertiesFile(file)
    else Map(name -> value)
  }

}

class PropsHolder(val elemName: String,
                  val baseDir: File)
    extends ListHolder[Prop] {

  def read = {
    case p => new Prop(p)
  }

  def toMap = Map(children.flatMap(_.toMap): _*)

}
