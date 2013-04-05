package pro.savant.circumflex
package cluster

import core._, xml._, cache._
import java.io._
import java.util.{Properties, Enumeration => JEnum}
import collection.mutable.HashMap
import collection.JavaConversions._

class Prop(val name: String) extends TextHolder {

  def elemName = name

}

class PropsHolder(val elemName: String)
    extends ListHolder[Prop] {

  def read = {
    case p => new Prop(p)
  }

  def toMap = Map(children.map(p => p.name -> p.getOrElse("")): _*)

}

class PropsFile(val file: File)
    extends Cached {

  def expired = file.lastModified > createdAt.getTime

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
          CL_LOG.error("Error reading " + file.getAbsolutePath, e)
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