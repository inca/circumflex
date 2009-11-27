package ru.circumflex.core

import collection.mutable.HashMap
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}

class CircumflexContext(val request: HttpServletRequest,
                        val response: HttpServletResponse,
                        val filter: AbstractCircumflexFilter)
    extends HashModel {

  val params = new HashMap[String, Any]
  val stringHeaders = new HashMap[String, String]
  val dateHeaders = new HashMap[String, Long]
  var statusCode = 200
  var contentType = "text/html"

  def get(key: String) = params.get(key) match {
    case Some(value) => Some(value)
    case _ => {
      val value = request.getParameter(key)
      if (value == null) None
      else Some(value)
    }
  }

  def stringParam(key: String): Option[String] = params.get(key) match {
    case Some(value: String) => Some(value)
    case _ => {
      val value = request.getParameter(key)
      if (value == null) None
      else Some(value)
    }
  }

  def noCache() = {
    stringHeaders += "Pragma" -> "no-cache"
    stringHeaders += "Cache-Control" -> "no-store"
    dateHeaders += "Expires" -> 0l
  }

  def +=(pair: Pair[String, Any]): Unit = params += pair

  def ++=(map: Map[String, Any]): Unit = params ++= map

  def apply(key: String): Option[Any] = get(key)

}


object Circumflex {
  private val threadLocalContext = new ThreadLocal[CircumflexContext]

  def ctx = threadLocalContext.get

  def initContext(req: HttpServletRequest,
                  res: HttpServletResponse,
                  filter: AbstractCircumflexFilter) =
    threadLocalContext.set(new CircumflexContext(req, res, filter))

  def destroyContext() = threadLocalContext.set(null)
}