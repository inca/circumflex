package circumflex.core

import javax.servlet.http.HttpServletRequest

class RequestContext(val request: HttpServletRequest, val params: Map[String, Object]) {

  def this(request: HttpServletRequest) = this(request, Map[String, Object]())

  def +(pair: Pair[String, Object]) = new RequestContext(request, params + pair)

  def ++(map: Map[String, Object]) = new RequestContext(request, params ++ map)

  def apply(key: String):Option[Object] = params.get(key)

}