package ru.circumflex.freemarker

import ru.circumflex.md.Markdown
import ru.circumflex.core._
import ru.circumflex.web._
import freemarker.template._
import freemarker.core.Environment
import freemarker.cache._
import java.io.StringWriter

class FTL extends Configuration {

  // Loaders

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

  // Defaults

  setObjectWrapper(new ScalaObjectWrapper())
  setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER)
  setDefaultEncoding("utf-8")
  setSharedVariable("md", MarkdownDirective)

  // Rendering methods

  def ftl(template: String, data: Any = ctx, statusCode: Int = -1): Nothing = {
    if (statusCode != -1)
      response.statusCode(statusCode)
    response.body(r => getTemplate(template).process(data, r.getWriter)).flush_!
  }
  def renderFtl(template: String, params: (String, Any)*): String =
    ftl(template, Map[String, Any](params: _*))
  def renderFtl(template: String, root: Any = ctx): String = {
    val result = new StringWriter
    getTemplate(template).process(root, result)
    return result.toString
  }
}

/*! The default FreeMarker configuration implies following:

  * templates are loaded from `${webapp-root}/templates directory` or,
  if failed, from webapp classpath;
  * all template errors result in exception to be thrown to controller;
  * character encoding defaults to UTF-8;
  * the `ScalaObjectWrapper` is used for Scala core types.
*/
object FTL extends FTL {
  try {
    addLoader(new WebappTemplateLoader(servletContext, "/templates"))
  } catch {
    case e =>
      CX_LOG.warn("Not running in webapp context.")
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
