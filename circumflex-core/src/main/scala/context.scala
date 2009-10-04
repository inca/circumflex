package circumflex.core

import javax.servlet.http.HttpServletRequest

class RouteContext(val request: HttpServletRequest, val params: Map[String, Object]) {

  var stringHeaders: Map[String, String] = Map()
  var dateHeaders: Map[String, Long] = Map()
  var statusCode = 200
  var contentType = "text/html"

  def this(request: HttpServletRequest) = this(request, Map[String, Object]())

  def +(pair: Pair[String, Object]) = new RouteContext(request, params + pair)

  def ++(map: Map[String, Object]) = new RouteContext(request, params ++ map)

  def ++(ctx: RouteContext) = new RouteContext(request, params ++ ctx.params)

  def apply(key: String):Option[Object] = params.get(key)

}