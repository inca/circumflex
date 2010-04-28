package ru.circumflex.freemarker

import _root_.freemarker.template.{TemplateExceptionHandler, Template, Configuration}
import _root_.freemarker.cache._
import ru.circumflex.core._
import javax.servlet.http.HttpServletResponse

trait FreemarkerHelper {

  def freemarkerConf: Configuration = DefaultConfiguration

  def ftl(template: String): HttpResponse =
    new FreemarkerResponse(freemarkerConf.getTemplate(template))

  def ftl(template: String, statusCode: Int): HttpResponse = {
    context.statusCode = statusCode
    ftl(template)
  }

}

object FTL extends FreemarkerHelper

case class FreemarkerResponse(val template:Template) extends HttpResponse {
  override def apply(response: HttpServletResponse) = {
    var ftlCtx = context;
    ftlCtx('context) = context;
    super.apply(response)
    template.process(ftlCtx, response.getWriter)
  }
}

/**
 * This FreeMarker configuration implies following:
 * <ul>
 * <li>templates are loaded from ${webapp-Context}/templates directory or,
 * if failed, from webapp classpath;</li>
 * <li>all template errors result in exception to be thrown to controller;</li>
 * <li>character encoding defaults to UTF-8;</li>
 * <li>use Circumflex default object wrapper for Scala core types.</li>
 * </ul>
 */
object DefaultConfiguration extends Configuration {
  var loaders: Seq[TemplateLoader] = Nil
  try {
    loaders ++= List(new WebappTemplateLoader(
    context.request.getSession.getServletContext, "/templates"))
  } catch {
    case e => cxLog.debug("Not running in Servlet Context.", e)
  }
  loaders ++= List(new ClassTemplateLoader(getClass, "/"))
  setTemplateLoader(new MultiTemplateLoader(loaders.toArray))
  setObjectWrapper(new ScalaObjectWrapper())
  setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER)
  setDefaultEncoding("utf-8")
}
