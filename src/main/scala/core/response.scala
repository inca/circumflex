package circumflex.core

import _root_.freemarker.template.Template
import java.util.HashMap
import javax.servlet.http.HttpServletResponse

/**
 * Author: incarnate
 * Date: Aug 30, 2009
 * Time: 8: 48: 13 PM
 */

trait HttpResponse {
  def apply(response: HttpServletResponse): Unit
}

case class ErrorResponse(val errorCode: Int, val msg: String) extends HttpResponse {
  def this(errorCode: Int) = this(errorCode, "")  
  def apply(response: HttpServletResponse) =
    response.sendError(errorCode, msg)
}

object ErrorResponse {
  def apply(errorCode: Int) = new ErrorResponse(errorCode)
}

case class RedirectResponse(val url: String) extends HttpResponse {
  def apply(response: HttpServletResponse) =
    response.sendRedirect(url)
}

case class TextResponse(val text: String) extends HttpResponse {
  def apply(response: HttpServletResponse) = {
    response.setCharacterEncoding("UTF-8")
    response.setContentType("text/plain")
    response.getWriter.print(text)
  }
}

case class EmptyResponse extends HttpResponse {
  def apply(response: HttpServletResponse) = {}
}

case class FreemarkerTemplateResponse(val template:Template, val context:Map[String, Any]) extends HttpResponse {
  def apply(response: HttpServletResponse) = {
    response.setCharacterEncoding("UTF-8")
    response.setContentType("text/html")    //TODO fixme
    // We add ctx variable for conveniance
    template.process(context + ("ctx" -> context), response.getWriter)
  }
}