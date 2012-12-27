package pro.savant.circumflex

import core._, web._
import java.io._
import _root_.freemarker.template._

/*!# The `ftl` package

Package `ftl` contains rendering methods, `ftl` for use in Circumflex Web Framework and
`ftl2xxx` to render an FTL template into `xxx`. It also maintains Freemarker configuration,
use `ftlConfig` to access it if you need custom operations, or use `ftl.configuration`
configuration parameter to provide your own implementation of FreeMarker `Configuration`.

You should import this package to use Circumflex FreeMarker Helper in your application:

    import pro.savant.circumflex.freemarker._
 */
package object freemarker {

  val FTL_LOG = new Logger("pro.savant.circumflex.ftl")

  val DEFAULT_FTL_CONFIGURATION: Configuration =
    cx.instantiate[Configuration](
      "ftl.configuration", new DefaultConfiguration)

  def ftlConfig = ctx.getAs[Configuration]("ftl.configuration")
      .getOrElse(DEFAULT_FTL_CONFIGURATION)

  def ftl(template: String, data: Any = ctx): Nothing =
    response.body { r =>
      ftlConfig
          .getTemplate(template)
          .process(data, r.getWriter)
    }.flush()

  def ftl2string(template: String, root: Any = ctx): String = {
    val result = new StringWriter
    ftlConfig.getTemplate(template).process(root, result)
    result.toString
  }

  /*!# Configuring Object Wrapper

  Circumflex FreeMarker Helper provides facilities to make Scala objects available inside
  FreeMarker templates. These facilities are implemented inside `ScalaObjectWrapper`.

  There are couple of things which can be configured:

  * by default, all public fields can be resolved on any object (e.g. `${myObj.myField}`);
    to disable this, set `ftl.wrapper.resolveFields` configuration parameter to `false`;
  * by default, all public methods can be resolved on any object (e.g. `${myObj.myMethod("Hello")}`);
    to disable this, set `ftl.wrapper.resolveMethods` configuration parameter to `false`;
  * you can set `ftl.wrapper.delegateToDefault` configuration parameter to `true` in order to
    delegate resolving to FreeMarker's default object wrapper (`ObjectWrapper.DEFAULT_WRAPPER`);
    this can be useful if you work with Java types in your Scala applications (e.g. Java lists or
    maps); by default the delegation does not occur (`null` is returned if resolving fails).

  */
  val resolveFields = cx.get("ftl.wrapper.resolveFields") match {
    case Some(b: Boolean) => b
    case Some(s: String) => s.toBoolean
    case _ => true
  }
  val resolveMethods = cx.get("ftl.wrapper.resolveMethods") match {
    case Some(b: Boolean) => b
    case Some(s: String) => s.toBoolean
    case _ => true
  }
  val delegateToDefault = cx.get("ftl.wrapper.delegateToDefault") match {
    case Some(b: Boolean) => b
    case Some(s: String) => s.toBoolean
    case _ => false
  }

}
