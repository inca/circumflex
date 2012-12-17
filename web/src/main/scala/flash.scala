package pro.savant.circumflex
package web

import core._
import collection.Iterator
import collection.mutable.Map

/*!# Flash API

The `flash` object provides a way to pass temporary objects between requests.

{.warning}
This API has nothing to do with Adobe Flash™.

Flash variables are stored in session until first accessed.
*/
object flash extends Map[String, Any] with KeyValueCoercion {

  val SESSION_KEY = "cx.flash"

  protected def flashMap = session
      .getOrElse(SESSION_KEY, Map[String, Any]())
      .asInstanceOf[Map[String, Any]]

  def +=(kv: (String, Any)): this.type = {
    session(SESSION_KEY) = flashMap + (kv)
    this
  }

  def -=(key: String): this.type = {
    session(SESSION_KEY) = flashMap - key
    this
  }

  def iterator: Iterator[(String, Any)] = flashMap.iterator

  def get(key: String): Option[Any] = {
    val m = flashMap
    flashMap.get(key) map { v =>
      session(SESSION_KEY) = m - key
      v
    }
  }

  override def contains(key: String): Boolean = flashMap.contains(key)
}