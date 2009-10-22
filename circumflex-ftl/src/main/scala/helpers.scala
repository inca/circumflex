package ru.circumflex.freemarker

import core.{HttpResponse, RouteContext, ContextAware}
import _root_.freemarker.cache.ClassTemplateLoader
import _root_.freemarker.template.{Template, Configuration}

import javax.servlet.http.HttpServletResponse

trait FreemarkerHelper extends ContextAware {

  private val ftlCfg: Configuration = new Configuration();
  ftlCfg.setTemplateLoader(new ClassTemplateLoader(getClass, "/"))
  ftlCfg.setObjectWrapper(new ScalaObjectWrapper())
  ftlCfg.setDefaultEncoding("utf-8")

  def freemarkerConf: Configuration = ftlCfg

  def ftl(templateName:String) = {
    val template = freemarkerConf.getTemplate(templateName);
    new FreemarkerResponse(routeContext, template)
  }

}

case class FreemarkerResponse(ctx: RouteContext, val template:Template) extends HttpResponse(ctx) {
  override def apply(response: HttpServletResponse) = {
    var ftlCtx = ctx;
    ftlCtx += "ctx" -> ctx;
    ftlCtx += "textile" -> new TextileDirective
    super.apply(response)
    template.process(ftlCtx, response.getWriter)
  }
}