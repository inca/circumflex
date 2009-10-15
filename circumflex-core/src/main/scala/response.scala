package circumflex.core

import java.util.HashMap
import javax.servlet.http.HttpServletResponse

/**
 * Author: incarnate
 * Date: Aug 30, 2009
 * Time: 8: 48: 13 PM
 */

abstract class HttpResponse(val context: RouteContext) {
  def apply(response: HttpServletResponse) = {
    response.setCharacterEncoding("UTF-8")
    response.setContentType(context.contentType)
    response.setStatus(context.statusCode)
    context.stringHeaders.foreach(p => { response.setHeader(p._1, p._2) })
    context.dateHeaders.foreach(p => { response.setDateHeader(p._1, p._2) })
  }
}

case class EmptyResponse(ctx: RouteContext) extends HttpResponse(ctx)

case class ErrorResponse(ctx: RouteContext, val errorCode: Int, val msg: String) extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = response.sendError(errorCode, msg)
}

case class RedirectResponse(ctx: RouteContext, val url: String) extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = response.sendRedirect(url)
}

case class TextResponse(ctx: RouteContext, val text: String) extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = {
    super.apply(response)
    response.getWriter.print(text)
  }
}