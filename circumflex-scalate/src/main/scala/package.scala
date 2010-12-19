package ru.circumflex

import core._, web._
import org.fusesource.scalate._
import org.fusesource.scalate.servlet._
import java.util.Enumeration
import javax.servlet.ServletContext

/*!# The `scalate` package

Package `scalate` contains rendering methods and configuration objects
for [Scalate Templating Engine](http://scalate.fusesource.org/).

You should import this package if you intend to use Scalate in your web application:

    import ru.circumflex.scalate._

You can also use aliased imports to prevent collisions between method names:

    import ru.circumflex.{scalate => sc}    // import under alias "sc"
    sc.render("/path/to/template.ssp")      // access members

Here's the example usage from Circumflex Web Application:

    import ru.circumflex._, core._, web._, scalate._

    class Main extends RequestRouter {
       get("/hello/:name") = render("/templates/hello.ssp")
    }

*/
package object scalate {

  object defaultConfiguration extends Config {
    def getServletContext = servletContext
    def getName = getServletContext.getServletContextName
    def getInitParameterNames = getServletContext.getInitParameterNames
    def getInitParameter(name: String) = getServletContext.getInitParameter(name)
  }

  val defaultEngine = new ServletTemplateEngine(defaultConfiguration)
  defaultEngine.bindings ::= Binding("ctx", "ru.circumflex.core.Context", true, None, "val", true)

  val engine = cx.instantiate[TemplateEngine]("scalate.engine", defaultEngine)

  def acquireRenderContext: RenderContext = {
    val rctx = new ServletRenderContext(engine, response.raw.getWriter, request.raw, response.raw, servletContext)
    // add context parameters
    ctx.foreach(p => rctx.attributes(p._1) = p._2)
    rctx.attributes("ctx") = ctx
    return rctx
  }

  def render(template: String,
             statusCode: Int = 200,
             layout: Boolean = true): Nothing = {
    response.statusCode(statusCode)
    acquireRenderContext.include(template, layout)
    response.flush_!
  }

  def view(view: String, it: AnyRef): Nothing = {
    acquireRenderContext.view(it, view)
    response.flush_!
  }

}