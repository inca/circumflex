package pro.savant.circumflex
package web

import core._
import collection.mutable._

object partial {

  protected def _paramsMap =
    ctx.getOrElseUpdate("partial.params", new HashMap[String, String])
        .asInstanceOf[HashMap[String, String]]

  def paramsMap = _paramsMap.toMap

  def addParam(key: String, value: String): this.type = {
    _paramsMap += key -> value
    this
  }

  def removeParam(key: String): this.type = {
    _paramsMap -= key
    this
  }

  def recovers = ctx.getAs[ListBuffer[() => Unit]]("partial.recovers")
      .getOrElse {
    val buffer = new ListBuffer[() => Unit]
    ctx.update("partial.recovers", buffer)
    buffer
  }

  def apply(actions: => Unit): Nothing = {
    if (!request.isXHR) sendError(404)
    try {
      actions
    } catch {
      case e: ValidationException =>
        notices.addErrors(e.errors)
        recovers.foreach(_.apply())
      case e: ResponseSentMarker =>
        recovers.foreach(_.apply())
    }
    sendJson("{" + toJsonString + "}")
  }

  def addRecover(func: () => Unit) {
    recovers += func
  }

  def toJsonString = {
    val sb = new StringBuilder
    sb.append(notices.toJsonString)
    val params = paramsMap.toSeq ++
        ctx.getString("redirect").map(s => "redirect" -> s).toSeq
    if (params.size > 0) {
      sb.append(",")
      sb.append(web.toJsonString(params: _*))
    }
    sb.toString
  }

}