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

package ru.circumflex.freemarker

import _root_.freemarker.cache.{MultiTemplateLoader, WebappTemplateLoader, ClassTemplateLoader}
import _root_.freemarker.template.{TemplateExceptionHandler, Template, Configuration}
import ru.circumflex.core._
import Circumflex._
import javax.servlet.http.HttpServletResponse
import org.slf4j.LoggerFactory

trait FreemarkerHelper {

  def freemarkerConf: Configuration = DefaultConfiguration

  def ftl(template: String): HttpResponse = try {
      return new FreemarkerResponse(freemarkerConf.getTemplate(template))
    } catch {
      case e =>
        log.error("Could not process FreeMarker template.", e)
        return new ErrorResponse(404, "The requested resource could not be found.")
    }
  

  def ftl(template: String, statusCode: Int): HttpResponse = {
    ctx.statusCode = statusCode
    ftl(template)
  }


}

case class FreemarkerResponse(val template:Template) extends HttpResponse {
  override def apply(response: HttpServletResponse) = {
    var ftlCtx = ctx;
    ftlCtx += "ctx" -> ctx;
    ftlCtx += "textile" -> new TextileDirective
    super.apply(response)
    template.process(ftlCtx, response.getWriter)
  }
}

/**
 * This FreeMarker configuration implies following:
 * <ul>
 * <li>templates are loaded from ${webapp-context}/templates directory or,
 * if failed, from webapp classpath;</li>
 * <li>all template errors result in exception to be thrown to controller;</li>
 * <li>character encoding defaults to UTF-8;</li>
 * <li>use Circumflex default object wrapper for Scala core types.</li>
 * </ul>
 */
object DefaultConfiguration extends Configuration {
  val loaderServletContext = new WebappTemplateLoader(
    Circumflex.ctx.request.getSession.getServletContext, "/templates")
  val loaderClassPath = new ClassTemplateLoader(getClass, "/")

  setTemplateLoader(new MultiTemplateLoader(List(loaderServletContext, loaderClassPath).toArray))
  setObjectWrapper(new ScalaObjectWrapper())
  setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER)
  setDefaultEncoding("utf-8")
}
