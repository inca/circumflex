package ru.circumflex.core


import javax.servlet.http.{HttpServletResponse, HttpServletRequest}

class RouteContext(val request: HttpServletRequest,
                   val response: HttpServletResponse,
                   val filter: AbstractCircumflexFilter,
                   val params: Map[String, Any])
    extends HashModel {

  var stringHeaders: Map[String, String] = Map()
  var dateHeaders: Map[String, Long] = Map()
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

  def +(pair: Pair[String, Any]) = new RouteContext(request, response, filter, params + pair)

  def ++(map: Map[String, Any]) = new RouteContext(request, response, filter, params ++ map)

  def apply(key: String): Option[Any] = get(key)

}