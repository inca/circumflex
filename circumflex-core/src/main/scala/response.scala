/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

package ru.circumflex.core

import java.io.OutputStream
import javax.servlet.http.HttpServletResponse

/**
 * Author: incarnate
 * Date: Aug 30, 2009
 * Time: 8: 48: 13 PM
 */

trait HttpResponse extends ContextAware {
  def apply(response: HttpServletResponse) = {
    response.setCharacterEncoding("UTF-8")
    response.setContentType(ctx.contentType)
    response.setStatus(ctx.statusCode)
    ctx.stringHeaders.foreach(p => { response.setHeader(p._1, p._2) })
    ctx.dateHeaders.foreach(p => { response.setDateHeader(p._1, p._2) })
  }
}

case class EmptyResponse extends HttpResponse

case class ErrorResponse(val errorCode: Int, val msg: String) extends HttpResponse {
  override def apply(response: HttpServletResponse) = response.sendError(errorCode, msg)
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

case class DirectStreamResponse(val streamingFunc: OutputStream => Unit)
    extends HttpResponse {
  override def apply(response: HttpServletResponse) = {
    super.apply(response)
    streamingFunc(response.getOutputStream)
  }
}