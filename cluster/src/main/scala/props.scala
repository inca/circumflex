package pro.savant.circumflex
package cluster

import core._, xml._, cache._
import java.io._
import java.util.{Properties, Enumeration => JEnum}
import collection.mutable.HashMap
import collection.JavaConversions._

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
      new PropsFile(file).toMap
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

class PropsFile(val file: File) {

  def toMap: Map[String, String] = {
    val result = new HashMap[String, String]
    if (file.isFile) {
      val props = new Properties
      val is = new FileInputStream(file)
      val buf = new BufferedInputStream(is)
      try {
        props.load(buf)
      } catch {
        case e: Exception =>
          CL_LOG.error("Error reading " + file.getCanonicalPath, e)
      } finally {
        is.close()
        buf.close()
      }
      props.keys.asInstanceOf[JEnum[String]].foreach { k =>
        result += k -> props.getProperty(k)
      }
    }
    result.toMap
  }

}