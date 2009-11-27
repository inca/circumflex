package ru.circumflex.core

import java.io.{OutputStream, InputStream}
import java.util.HashMap
import javax.servlet.http.HttpServletResponse

/**
 * Author: incarnate
 * Date: Aug 30, 2009
 * Time: 8: 48: 13 PM
 */

abstract class HttpResponse(val context: CircumflexContext) {
  def apply(response: HttpServletResponse) = {
    response.setCharacterEncoding("UTF-8")
    response.setContentType(context.contentType)
    response.setStatus(context.statusCode)
    context.stringHeaders.foreach(p => { response.setHeader(p._1, p._2) })
    context.dateHeaders.foreach(p => { response.setDateHeader(p._1, p._2) })
  }
}

case class EmptyResponse(ctx: CircumflexContext) extends HttpResponse(ctx)

case class ErrorResponse(ctx: CircumflexContext, val errorCode: Int, val msg: String) extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = response.sendError(errorCode, msg)
}

case class RedirectResponse(ctx: CircumflexContext, val url: String) extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = response.sendRedirect(url)
}

case class TextResponse(ctx: CircumflexContext, val text: String) extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = {
    super.apply(response)
    response.getWriter.print(text)
  }
}

case class BinaryResponse(ctx: CircumflexContext, val data: Array[Byte]) extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = {
    super.apply(response)
    response.getOutputStream.write(data)
  }
}

case class DirectStreamResponse(ctx: CircumflexContext, val streamingFunc: OutputStream => Unit)
    extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = {
    super.apply(response)
    streamingFunc(response.getOutputStream)
  }
}