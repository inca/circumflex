package circumflex.core

import _root_.freemarker.template.Template
import java.util.HashMap
import javax.servlet.http.HttpServletResponse

/**
 * Author: incarnate
 * Date: Aug 30, 2009
 * Time: 8: 48: 13 PM
 */

abstract class HttpResponse(val context: RequestContext) {
  def apply(response: HttpServletResponse) = {
    response.setCharacterEncoding("UTF-8")
    response.setContentType("text/html")
  }
}

case class ErrorResponse(ctx: RequestContext, val errorCode: Int, val msg: String) extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = response.sendError(errorCode, msg)
}

case class RedirectResponse(ctx: RequestContext, val url: String) extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = response.sendRedirect(url)
}

case class TextResponse(ctx: RequestContext, val text: String) extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = {
    super.apply(response)
    response.getWriter.print(text)
  }
}

case class EmptyResponse(ctx: RequestContext) extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = {}
}

case class FreemarkerResponse(ctx: RequestContext, val template:Template) extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = {
    super.apply(response)
    template.process(ctx + ("ctx" -> ctx), response.getWriter)
  }
}