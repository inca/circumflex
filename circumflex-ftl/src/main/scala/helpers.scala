package ru.circumflex.freemarker

import _root_.freemarker.template.{TemplateExceptionHandler, Template, Configuration}
import core.{HttpResponse, CircumflexContext, ContextAware}
import _root_.freemarker.cache.ClassTemplateLoader
import javax.servlet.http.HttpServletResponse

trait FreemarkerHelper extends ContextAware {

  protected val ftlCfg: Configuration = new Configuration();
  ftlCfg.setTemplateLoader(new ClassTemplateLoader(getClass, "/"))
  ftlCfg.setObjectWrapper(new ScalaObjectWrapper())
  ftlCfg.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER)
  ftlCfg.setDefaultEncoding("utf-8")

  def freemarkerConf: Configuration = ftlCfg

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