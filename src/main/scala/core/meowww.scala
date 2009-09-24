package org.meowww.core

import _root_.freemarker.cache.ClassTemplateLoader
import _root_.freemarker.template.Configuration
import java.io.File
import java.lang.reflect.{InvocationTargetException, Constructor}
import java.util.Enumeration
import javax.servlet._
import http.{HttpServletResponse, HttpServletRequest}
import freemarker._

class MeowwwFilter extends Filter {

  var config: Config = null;

  def init(cfg: FilterConfig) = {
    val paramNames = cfg.getInitParameterNames.asInstanceOf[Enumeration[String]]
    def extractInitParameters(map: Map[String, String]): Map[String, String] = {
      paramNames.hasMoreElements match {
        case true => {
          val name = paramNames.nextElement
          val value = cfg.getInitParameter(name)
          extractInitParameters(map + (name -> value))
        }
        case false => map
      }
    }
    config = new Config(extractInitParameters(Map()))
  }

  def destroy = {}

  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain) = (req, res) match {
    case (req: HttpServletRequest, res: HttpServletResponse) => {
      if (config.mode == Development) println(req)
      try {
        config.routerConstructor.newInstance(req, config)
        ErrorResponse(404)(res)
      } catch {
        case e: InvocationTargetException if e.getCause.isInstanceOf[RouteMatchedException] => {
          e.getCause.asInstanceOf[RouteMatchedException].response(res)
          if (config.mode == Development) println(res)
        } case e => {
          ErrorResponse(500, e.getMessage)(res)
          e.printStackTrace
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
      .getConstructor(classOf[HttpServletRequest], classOf[Config])
      .asInstanceOf[Constructor[RequestRouter]]
  val freemarkerConf = new Configuration();
  freemarkerConf.setTemplateLoader(new ClassTemplateLoader(getClass, "/"))
  freemarkerConf.setObjectWrapper(new ScalaObjectWrapper())
}

trait Mode
object Production extends Mode
object Development extends Mode