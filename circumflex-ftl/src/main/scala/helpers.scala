package ru.circumflex.freemarker

import _root_.freemarker.cache._
import _root_.ru.circumflex.md.Markdown
import ru.circumflex.core._
import javax.servlet.http.HttpServletResponse
import freemarker.template._
import freemarker.core.Environment
import java.io.StringWriter

trait FreemarkerHelper {
  protected var _freemarkerConf: Configuration = DefaultConfiguration
  def freemarkerConf = _freemarkerConf
  def setConfiguration(c: Configuration) = {
    _freemarkerConf = c
  }
  def ftl(template: String): Nothing =
    throw new RouteMatchedException(new FreemarkerResponse(freemarkerConf.getTemplate(template)))
  def ftl(template: String, statusCode: Int): Nothing = {
    ctx.statusCode = statusCode
    ftl(template)
  }
  def ftl(template: String, params: Pair[String, Any]*): String =
    ftl(template, Map[String, Any](params: _*))
  def ftl(template: String, root: Any): String = {
    val result = new StringWriter
    freemarkerConf.getTemplate(template).process(root, result)
    return result.toString
  }
}

object FTL extends FreemarkerHelper

case class FreemarkerResponse(val template: Template) extends HttpResponse {
  override def apply(response: HttpServletResponse) = {
    var ftlCtx = ctx;
    ftlCtx('context) = ctx;
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
    ctx.request.getSession.getServletContext, "/templates"))
  } catch {
    case e => cxLog.debug("Not running in Servlet Context.", e)
  }
  loaders ++= List(new ClassTemplateLoader(getClass, "/"))
  setTemplateLoader(new MultiTemplateLoader(loaders.toArray))
  setObjectWrapper(new ScalaObjectWrapper())
  setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER)
  setDefaultEncoding("utf-8")
  setSharedVariable("md", MarkdownDirective)
}

object MarkdownDirective extends TemplateDirectiveModel {
  def execute(env: Environment,
              params: java.util.Map[_, _],
              loopVars: Array[TemplateModel],
              body: TemplateDirectiveBody) = {
    val nested = new StringWriter
    body.render(nested)
    env.getOut.write(Markdown(nested.toString))
  }
}
