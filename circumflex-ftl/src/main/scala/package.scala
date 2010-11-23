package ru.circumflex

import core._, freemarker._, web._
import java.io._
import _root_.freemarker.template._

/*!# The `ftl` package

Package `ftl` contains rendering methods, `ftl` for use in Circumflex Web Framework and
`ftl2xxx` to render an FTL template into `xxx`. It also maintains Freemarker configuration,
use `ftlConfig` to access it if you need custom operations, or use `ftl.configuration`
configuration parameter to provide your own implementation of FreeMarker `Configuration`.

You should import this package to use Circumflex FreeMarker Helper in your application:

    import ru.circumflex.freemarker._
 */
package object freemarker {

  val ftlConfig: Configuration = cx.instantiate[Configuration](
    "ftl.configuration", new DefaultConfiguration)

  /**
   * Renders specified `template` directly into current response;
   * must be invoked inside a router definition.
   */
  def ftl(template: String, data: Any = ctx): Nothing =
    response.body(r => ftlConfig.getTemplate(template).process(data, r.getWriter)).flush_!

  /**
   * Renders specified `template` and returns produced content.
   */
  def ftl2string(template: String, root: Any = ctx): String = {
    val result = new StringWriter
    ftlConfig.getTemplate(template).process(root, result)
    return result.toString
  }

}
