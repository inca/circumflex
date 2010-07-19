package ru.circumflex.freemarker

import _root_.freemarker.cache._
import _root_.ru.circumflex.md.Markdown
import ru.circumflex.core._
import freemarker.template._
import freemarker.core.Environment
import java.io.StringWriter

class FTL extends Configuration {

  // ## Loaders
  protected var _loaders: Seq[TemplateLoader] = Nil
  def loaders = _loaders
  def addLoader(loader: TemplateLoader): this.type = {
    _loaders ++= List(loader)
    setTemplateLoader(new MultiTemplateLoader(loaders.toArray))
    return this
  }
  def setLoaders(ldrs: TemplateLoader*): this.type = {
    _loaders = ldrs.toList
    setTemplateLoader(new MultiTemplateLoader(loaders.toArray))
    return this
  }

  // ## Defaults

  setObjectWrapper(new ScalaObjectWrapper())
  setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER)
  setDefaultEncoding("utf-8")
  setSharedVariable("md", MarkdownDirective)

  // ## Rendering methods

  def ftl(template: String): Nothing =
    throw new RouteMatchedException(new WriterResponse(w => getTemplate(template).process(ctx, w)))
  def ftl(template: String, statusCode: Int): Nothing = {
    ctx.statusCode = statusCode
    ftl(template)
  }
  def ftl(template: String, params: Pair[String, Any]*): String =
    ftl(template, Map[String, Any](params: _*))
  def ftl(template: String, root: Any): String = {
    val result = new StringWriter
    getTemplate(template).process(root, result)
    return result.toString
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
object FTL extends FTL {
  try {
    addLoader(new WebappTemplateLoader(ctx.request.getSession.getServletContext, "/templates"))
  } catch {
    case e =>
      cxLog.warn("Not running in webapp context.")
  }
  addLoader(new ClassTemplateLoader(getClass, "/"))
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
