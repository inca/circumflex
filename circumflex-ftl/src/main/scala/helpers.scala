package ru.circumflex.freemarker

import _root_.freemarker.template.{TemplateExceptionHandler, Template, Configuration}
import _root_.freemarker.cache._
import ru.circumflex.core._
import Circumflex._
import javax.servlet.http.HttpServletResponse
import org.slf4j.LoggerFactory

trait FreemarkerHelper {

  def freemarkerConf: Configuration = DefaultConfiguration

  def ftl(template: String): HttpResponse =
    new FreemarkerResponse(freemarkerConf.getTemplate(template))

  def ftl(template: String, statusCode: Int): HttpResponse = {
    ctx.statusCode = statusCode
    ftl(template)
  }

}

object FTL extends FreemarkerHelper

case class FreemarkerResponse(val template:Template) extends HttpResponse {
  override def apply(response: HttpServletResponse) = {
    var ftlCtx = ctx;
    ftlCtx += "ctx" -> ctx;
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
  var loaders: Seq[TemplateLoader] = Nil
  loaders ++= List(new ClassTemplateLoader(getClass, "/"))
  try {
    loaders ++= List(new WebappTemplateLoader(
    Circumflex.ctx.request.getSession.getServletContext, "/templates"))
  } catch {
    case e => log.debug("Not running in Servlet context.", e)
  }
  setTemplateLoader(new MultiTemplateLoader(loaders.toArray))
  setObjectWrapper(new ScalaObjectWrapper())
  setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER)
  setDefaultEncoding("utf-8")
}
