package circumflex
package web

import core._
import scala.collection.mutable.HashSet
import java.io.Serializable

case class Notice(kind: String, msg: Msg) extends Serializable {
  def key = msg.key
}

object notices {

  protected val KEY = "cx.notices"

  protected def hashSet: Option[HashSet[Notice]] =
    sessionOption.map { sess =>
      sess.getAs[HashSet[Notice]](KEY) getOrElse {
        val s = new HashSet[Notice]
        sess += KEY -> s
        s
      }
    }

  def get: Set[Notice] = {
    val result = hashSet.getOrElse(HashSet.empty).toSet
    sessionOption.map(_ -= KEY)
    result
  }

  protected def add(kind: String, msg: Msg): this.type = {
    hashSet.map(_ += Notice(kind, msg))
    this
  }

  protected def add(kind: String, msgs: Seq[Msg]): this.type = {
    hashSet.map(s => msgs.foreach(m => s += Notice(kind, m)))
    this
  }

  def addMsg(kind: String, key: String, params: (String, Any)*): this.type =
    add(kind, new Msg(key, params: _*))

  def addInfo(key: String, params: (String, Any)*) =
    addMsg("info", key, params: _*)

  def addWarn(key: String, params: (String, Any)*) =
    addMsg("warn", key, params: _*)

  def addError(key: String, params: (String, Any)*) =
    addMsg("error", key, params: _*)

  def addErrors(msgs: Seq[Msg]) = add("error", msgs)

  def toJsonString = "\"notices\":[" +
      get.map(n => "{" +
          web.toJsonString("kind" -> n.kind, "msg" -> n.msg.toString) +
          "}").mkString(",") + "]"
}