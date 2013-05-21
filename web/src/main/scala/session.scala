package pro.savant.circumflex
package web

import core._
import collection.mutable.Map
import collection.Iterator
import collection.JavaConversions._
import java.util.{Enumeration => JEnumeration}
import javax.servlet.http.{HttpSession => ServletSession}

class HttpSession extends Map[String, Any] with KeyValueCoercion {

  def rawSession: Option[ServletSession] = requestOption.flatMap { req =>
    val s = req.raw.getSession(false)
    if (s == null) None
    else Some(s)
  }

  def id: Option[String] = rawSession.map(_.getId)

  def +=(kv: (String, Any)): this.type = {
    requestOption.map(_.raw.getSession(true).setAttribute(kv._1, kv._2))
    this
  }

  def -=(key: String): this.type = {
    rawSession.map(_.removeAttribute(key))
    this
  }

  def iterator: Iterator[(String, Any)] = {
    rawSession.map(s =>
      s.getAttributeNames
          .asInstanceOf[JEnumeration[String]]
          .map(k => (k -> s.getAttribute(k)))
          .toIterator)
        .getOrElse(Iterator.empty)
  }

  def get(key: String): Option[Any] =
    rawSession.flatMap(s => any2option(s.getAttribute(key)))

  def invalidate(): this.type = {
    rawSession.map(_.invalidate())
    this
  }

}
