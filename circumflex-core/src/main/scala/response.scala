package ru.circumflex.core

import Circumflex._
import javax.servlet.http.{HttpServletResponse}
import javax.activation.MimetypesFileTypeMap
import org.apache.commons.io.IOUtils
import java.io.{FileInputStream, File, OutputStream}

/**
 * Represents an `HttpServletResponse` wrapper for committing responses.
 * Apply method sets character encoding, content type, status code and headers from
 * `CircumflexContext` and flushes any associated output.
 */
trait HttpResponse {
  /**
   * Applies character encoding, content type, status code and headers to
   * specified `HttpServletResponse` and flushes any associated output.
   * If specified response is committed, returns silently.
   */
  def apply(response: HttpServletResponse) = if (!response.isCommitted) {
    response.setCharacterEncoding("UTF-8")
    response.setContentType(ctx.contentType.getOrElse("text/html"))
    response.setStatus(ctx.statusCode)
    ctx.stringHeaders.foreach(p => { response.setHeader(p._1, p._2) })
    ctx.dateHeaders.foreach(p => { response.setDateHeader(p._1, p._2) })
  }
}

case class EmptyResponse extends HttpResponse

case class ErrorResponse(val errorCode: Int, val msg: String) extends HttpResponse {
  override def apply(response: HttpServletResponse) = {
    ctx.statusCode = errorCode
    response.sendError(errorCode, msg)
  }
}

case class RedirectResponse(val url: String) extends HttpResponse {
  override def apply(response: HttpServletResponse) = response.sendRedirect(url)
}

case class TextResponse(val text: String) extends HttpResponse {
  override def apply(response: HttpServletResponse) = {
    super.apply(response)
    response.getWriter.print(text)
  }
}

case class BinaryResponse(val data: Array[Byte]) extends HttpResponse {
  override def apply(response: HttpServletResponse) = {
    super.apply(response)
    response.getOutputStream.write(data)
  }
}

case class FileResponse(val file: File) extends HttpResponse {
  override def apply(response: HttpServletResponse) = {
    ctx.contentType = ctx.contentType match {
      case None =>          // determine mime type by extension
        Circumflex.mimeTypesMap.getContentType(file)
      case Some(ct) => ct   // use already provided one
    }
    super.apply(response)
    // transfer a file
    val is = new FileInputStream(file)
    try {
      IOUtils.copy(is, response.getOutputStream)
    } finally {
      is.close
    }
  }
}

case class DirectStreamResponse(val streamingFunc: OutputStream => Unit)
    extends HttpResponse {
  override def apply(response: HttpServletResponse) = {
    super.apply(response)
    streamingFunc(response.getOutputStream)
  }
}