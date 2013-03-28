package pro.savant.circumflex
package web

import core._
import collection.mutable.ListBuffer

object notices {

  case class Notice(kind: String, msg: Msg) {
    def key = msg.key
  }

  def buffer: ListBuffer[Notice] = {
    val b = flash.getAs[ListBuffer[Notice]]("notices")
        .getOrElse(new ListBuffer[Notice])
    flash.update("notices", b)
    b
  }

  def get: Seq[Notice] = flash.getAs[Seq[Notice]]("notices").getOrElse(Nil)

  protected def add(kind: String, msg: Msg): this.type = {
    buffer += Notice(kind, msg)
    this
  }

  protected def add(kind: String, msgs: Seq[Msg]): this.type = {
    msgs.foreach(m => add(kind, m))
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