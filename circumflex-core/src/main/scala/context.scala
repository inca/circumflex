package circumflex.core


import javax.servlet.http.{HttpServletResponse, HttpServletRequest}

class RouteContext(val request: HttpServletRequest,
                   val response: HttpServletResponse,
                   val filter: AbstractCircumflexFilter,
                   val params: Map[String, Object]) {

  var stringHeaders: Map[String, String] = Map()
  var dateHeaders: Map[String, Long] = Map()
  var statusCode = 200
  var contentType = "text/html"

  def stringParam(key: String): Option[String] = params.get(key) match {
    case Some(value: String) => Some(value)
    case _ => {
      val value = request.getParameter(key)
      if (value == null) None
      else Some(value)
    }
  }

  def +(pair: Pair[String, Object]) = new RouteContext(request, response, filter, params + pair)

  def ++(map: Map[String, Object]) = new RouteContext(request, response, filter, params ++ map)

  def apply(key: String):Option[Object] = params.get(key)

}