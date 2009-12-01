package ru.circumflex.freemarker

import _root_.freemarker.cache.{MultiTemplateLoader, WebappTemplateLoader, ClassTemplateLoader}
import _root_.freemarker.template.{TemplateExceptionHandler, Template, Configuration}
import core.{HttpResponse, CircumflexContext, ContextAware, Circumflex}
import javax.servlet.http.HttpServletResponse
import javax.servlet.ServletContext

trait FreemarkerHelper extends ContextAware {

  def freemarkerConf: Configuration = DefaultConfiguration

  def ftl(templateName:String) = {
    val template = freemarkerConf.getTemplate(templateName);
    new FreemarkerResponse(ctx, template)
  }

}

case class FreemarkerResponse(ctx: CircumflexContext, val template:Template) extends HttpResponse(ctx) {
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