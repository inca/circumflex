package ru.circumflex
package freemarker

import core._, web._
import markeven.BlockProcessor
import _root_.freemarker.template._
import _root_.freemarker.core.Environment
import _root_.freemarker.cache._
import java.io.{File, StringWriter}

/*!# Default FreeMarker Configuration

The default FreeMarker configuration implies following:

* templates are loaded from `${webapp-root}/templates directory` or,
if failed, from application classpath;
* all template errors result in exception to be thrown to controller;
* character encoding defaults to UTF-8;
* the `ScalaObjectWrapper` is used for Scala core types.

You can alter template loading dynamically using `addLoader` and `setLoaders`
methods, but in general this is only acceptable in initialization code. In any
case make sure you know what you are doing first.
*/
class DefaultConfiguration extends Configuration {

  // Loaders

  protected var _loaders: Seq[TemplateLoader] = Nil
  def loaders = _loaders
  def addLoader(loader: TemplateLoader): this.type = {
    _loaders ++= List(loader)
    setTemplateLoader(new MultiTemplateLoader(loaders.toArray))
    this
  }
  def setLoaders(ldrs: TemplateLoader*): this.type = {
    _loaders = ldrs.toList
    setTemplateLoader(new MultiTemplateLoader(loaders.toArray))
    this
  }

  // Defaults

  setObjectWrapper(new ScalaObjectWrapper())
  setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER)
  setDefaultEncoding("utf-8")
  setSharedVariable("me", MarkevenDirective)

  try {
    addLoader(new WebappTemplateLoader(servletContext, "/templates"))
  } catch {
    case e: Exception =>
      CX_LOG.warn("Not running in webapp context.")
  }
  addLoader(new ClassTemplateLoader(getClass, "/"))
  addLoader(new FileTemplateLoader(new File("/")))
}

object MarkevenDirective extends TemplateDirectiveModel {
  def execute(env: Environment,
              params: java.util.Map[_, _],
              loopVars: Array[TemplateModel],
              body: TemplateDirectiveBody) {
    val nested = new StringWriter
    body.render(nested)
    new BlockProcessor(env.getOut, markeven.conf).process(nested.toString)
  }
}
