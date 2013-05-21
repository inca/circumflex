package circumflex
package web

import core._
import collection.mutable.Map
import collection.Iterator
import collection.JavaConversions._
import java.util.{Enumeration => JEnumeration}
import javax.servlet.http.{HttpSession => ServletSession}
import java.io.Serializable

class HttpSession
    extends Map[String, Serializable]
    with KeyValueCoercion {

  def rawSession: Option[ServletSession] = requestOption.flatMap { req =>
    val s = req.raw.getSession(false)
    if (s == null) None
    else Some(s)
  }

  def id: Option[String] = rawSession.map(_.getId)

  def +=(kv: (String, Serializable)): this.type = {
    requestOption.map(_.raw.getSession(true).setAttribute(kv._1, kv._2))
    this
  }

  def -=(key: String): this.type = {
    rawSession.map(_.removeAttribute(key))
    this
  }

  def iterator: Iterator[(String, Serializable)] = {
    rawSession.map(s =>
      s.getAttributeNames
          .asInstanceOf[JEnumeration[String]]
          .flatMap(k => s.getAttribute(k) match {
        case s: Serializable => Some(k -> s)
        case _ => None
      }).toIterator).getOrElse(Iterator.empty)
  }

  def get(key: String): Option[Serializable] =
    rawSession.flatMap(sess => any2option(sess.getAttribute(key))) match {
      case Some(s: Serializable) => Some(s)
      case _ => None
    }

  def invalidate(): this.type = {
    rawSession.map(_.invalidate())
    this
  }

}
