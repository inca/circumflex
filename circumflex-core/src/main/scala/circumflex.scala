package circumflex.core

import _root_.freemarker.cache.ClassTemplateLoader
import _root_.freemarker.template.Configuration
import java.io.File
import java.lang.reflect.{InvocationTargetException, Constructor}
import java.util.Enumeration
import javax.servlet._
import http.{HttpServletResponse, HttpServletRequest}
import freemarker._
import org.slf4j.LoggerFactory
import util.matching.Regex

abstract class AbstractFilter extends Filter {

  var params: Map[String, String] = Map();

  def init(cfg: FilterConfig) = {
    val paramNames = cfg.getInitParameterNames.asInstanceOf[Enumeration[String]]
    while(paramNames.hasMoreElements) {
      val name = paramNames.nextElement
      val value = cfg.getInitParameter(name)
      params += name -> value
    }
  }

  def destroy = {}
}

class CircumflexFilter extends AbstractFilter {

  val log = LoggerFactory.getLogger("circumflex.core.filter")
  var config: Config = null;

  override def init(cfg: FilterConfig) = {
    super.init(cfg)
    config = new Config(params)
  }

  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain): Unit =
    (req, res) match {
      case (req: HttpServletRequest, res: HttpServletResponse) => {
        // Serve static content
        if (config.staticRegex.pattern.matcher(req.getRequestURI).matches) {
          chain.doFilter(req,res)
          return
        }
        // Process request with router
        if (config.mode == Development) println(req)
        req.setCharacterEncoding("UTF-8")
        try {
          config.routerConstructor.newInstance(req, res, config, new RouteContext(req))
          ErrorResponse(new RouteContext(req), 404, req.getRequestURI)(res)
        } catch {
          case e: InvocationTargetException if e.getCause.isInstanceOf[RouteMatchedException] => {
            e.getCause.asInstanceOf[RouteMatchedException].response match {
              case Some(response) => {
                response(res)
                if (config.mode == Development) println(res)
              } case _ => }
          } case e => {
            ErrorResponse(new RouteContext(req), 500, e.getMessage)(res)
            log.error("Controller threw an exception, see stack trace for details.", e)
          }
        }
      }
    }
}

class Config(val params: Map[String, String]) {

  val mode: Mode = params.get("mode") match {
    case Some(m) if m.toLowerCase.matches("dev.*") => Development
    case _ => Production;
  }

  val routerConstructor: Constructor[RequestRouter] = Class.forName(params("router"))
      .getConstructor(classOf[HttpServletRequest],
    classOf[HttpServletResponse],
    classOf[Config],
    classOf[RouteContext])
      .asInstanceOf[Constructor[RequestRouter]]

  val freemarkerConf = new Configuration();
  freemarkerConf.setTemplateLoader(new ClassTemplateLoader(getClass, "/"))
  freemarkerConf.setObjectWrapper(new ScalaObjectWrapper())
  freemarkerConf.setDefaultEncoding("utf-8")

  val staticRegex: Regex = params.get("staticRegex") match {
    case Some(sr) => sr.r
    case _ => "/static/.*".r
  }


}

trait Mode
object Production extends Mode
object Development extends Mode