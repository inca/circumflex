package circumflex.core

import javax.servlet.http.HttpServletRequest
import HttpRequest._

class HttpRequest(val request: HttpServletRequest, val params:Map[String, String]) {

  def this(request:HttpServletRequest) = this(request, Map())

  def paramOr(name:String, default:String):String =
    params.get(name) match {
      case Some(value) => value
      case None => {
        val value = request.getParameter(name);
        if (value == null) default else value
      }
    }

  def param(name:String) = paramOr(name, "")

}

object HttpRequest {

  def apply(request:HttpServletRequest) = new HttpRequest(request)
  def apply(request:HttpServletRequest, params:Map[String, String]) = new HttpRequest(request, params)
}