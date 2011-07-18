package ru.circumflex

import core._, web._
import org.fusesource.scalate._
import org.fusesource.scalate.servlet._

/*!# The `scalate` package

Package `scalate` contains rendering methods and configuration objects
for [Scalate Templating Engine][scalate-home].

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

Two methods are used for rendering: `render(template: String, statusCode: Int = 200, layout: Boolean = true)`
and `view(view: String, it: AnyRef)`. Consult [Scalate Documentation][scalate-doc] for more details.

By default, the `ServletTemplateEngine` is used which resolves templates from servlet context.
If you wish to use your own `TemplateEngine` implementation with the methods of `scalate` package,
just set the `scalate.engine` configuration parameter to fully-qualified class name of templage
engine implementation.

  [scalate-doc]: http://scalate.fusesource.org/documentation/user-guide.html
  [scalate-home]: http://scalate.fusesource.org/

*/
package object scalate {

  object defaultConfiguration extends Config {
    def getServletContext = servletContext
    def getName = getServletContext.getServletContextName
    def getInitParameterNames = getServletContext.getInitParameterNames
    def getInitParameter(name: String) = getServletContext.getInitParameter(name)
  }

  val defaultEngine = new ServletTemplateEngine(defaultConfiguration)
  defaultEngine.importStatements ::= "import ru.circumflex._, core._, web._;"

  val engine = cx.instantiate[TemplateEngine]("scalate.engine", defaultEngine)

  def acquireRenderContext: RenderContext = {
    val rctx = new ServletRenderContext(engine, response.raw.getWriter, request.raw, response.raw, servletContext)
    // add context parameters
    ctx.foreach(p => rctx.attributes(p._1) = p._2)
    rctx
  }

  def render(template: String,
             statusCode: Int = 200,
             layout: Boolean = true): Nothing = {
    response.statusCode(statusCode)
    acquireRenderContext.include(template, layout)
    response.flush()
  }

  def view(view: String, it: AnyRef): Nothing = {
    acquireRenderContext.view(it, view)
    response.flush()
  }

}