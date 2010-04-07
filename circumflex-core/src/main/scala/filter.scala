package ru.circumflex.core

import java.lang.reflect.InvocationTargetException
import java.util.regex.Pattern
import util.matching.Regex
import javax.servlet._
import http.{HttpServletResponse, HttpServletRequest}
import org.slf4j.LoggerFactory
import Circumflex._
import java.io.{FileNotFoundException, File}
import org.apache.commons.io.FilenameUtils._

/**
 * ## Circumflex Filters
 *
 * Provides a base class for Circumflex filter implementations.
 */
abstract class AbstractCircumflexFilter extends Filter {

  protected val log = LoggerFactory.getLogger("ru.circumflex.core")

  /**
   * Place your application initialization code here.
   * Does nothing by default.
   */
  def init(filterConfig: FilterConfig) = {}

  /**
   * Place your application shutdown code here.
   * Does nothing by default.
   */
  def destroy = {}

  /**
   * Determines, if a filter should process the request.
   * The default behavior is controlled by `cx.process_?` parameter:
   *
   *  * `String` or `Regex` or `Pattern`;
   *  * the filter processes request if URI *does not match* specified regex;
   *  * `HttpServletRequest => Boolean` or `() => Boolean` functions,
   *  the filter processes request depending on the result of function invocation.
   * </ul>
   */
  def isProcessed(req: HttpServletRequest): Boolean = Circumflex.cfg("cx.process_?") match {
    case Some(s: String) => !req.getRequestURI.toLowerCase.matches(s)
    case Some(r: Regex) => !req.getRequestURI.toLowerCase.matches(r.toString)
    case Some(p: Pattern) => !p.matcher(req.getRequestURI.toLowerCase).matches()
    case Some(func: Function0[Boolean]) => func.apply()
    case Some(func: Function1[HttpServletRequest, Boolean]) => func.apply(req)
    case _ => true
  }

  /**
   * Instantiates a `CircumflexContext` object, binds it to current request,
   * consults `isProcessed, whether the request should be processed
   * and delegates to high-level equivalent
   * `doFilter(CircumflexContext, FilterChain)`
   * if necessary.
   */
  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain): Unit =
    (req, res) match {
      case (req: HttpServletRequest, res: HttpServletResponse) =>
        // try to serve static first
        if (req.getMethod.equalsIgnoreCase("get") || req.getMethod.equalsIgnoreCase("head")) {
          val resource = new File(Circumflex.publicRoot, separatorsToSystem(req.getRequestURI))
          if (resource.isFile) {
            val publicUri = Circumflex.cfg("cx.public") match {
              case Some(s: String) => "/" + s.replaceAll("^/?(.*?)/?$", "$1")
              case _ => "/public"
            }
            req.getRequestDispatcher(publicUri + req.getRequestURI).forward(req, res)
            return
          }
        }
        if (isProcessed(req)) {
          // Instantiate a context if it does not yet exist and bind it thread-locally.
          if (ctx == null) Circumflex.initContext(req, res, this)
          // chain a call and make sure the context is destroyed afterwards
          try {
            doFilter(ctx, chain)
          } finally {
            Circumflex.destroyContext()
          }
        } else chain.doFilter(req, res)
      case _ =>
    }

  /**
   * Implementing classes should provide their filtering logic for processed requests.
   */
  def doFilter(ctx: CircumflexContext, chain: FilterChain): Unit

}

/**
 * Configures Circumflex-based web application and serves it's requests.
 * Web application should configure it according to JSR-154 (Java Servlet Specification) via
 * `your_webapp_root/WEB-INF/web.xml` (also known as Deployment Descriptor).
 */
class CircumflexFilter extends AbstractCircumflexFilter {

  val routerClass: Class[RequestRouter] = Circumflex.cfg("cx.router") match {
    case Some(s: String) => Class
            .forName(s, true, Circumflex.classLoader)
            .asInstanceOf[Class[RequestRouter]]
    case Some(c: Class[RequestRouter]) => c
    case _ => throw new CircumflexException("Could not initialize Request Router; " +
            "configure 'cx.router' properly.")
  }

  /**
   * Executed when no routes match current request.
   * Default behavior is to send 404 NOT FOUND.
   * You may override it, say, to call `chain.doFilter` to pass request
   * along the chain.
   */
  def onNoMatch(ctx: CircumflexContext, chain: FilterChain) =
    ErrorResponse(404, "The requested resource does not exist.")(ctx.response)

  /**
   * Executed when router throws an exception.
   * Default behavior is to send the `500 Internal Server Error` to client.
   */
  def onRouterError(e: Throwable, ctx: CircumflexContext, chain: FilterChain) = {
    log.error("Router threw an exception, see stack trace for details.", e)
    ErrorResponse(500, e.getMessage)(ctx.response)
  }

  /**
   * Executed when `java.io.FileNotFoundException`
   * is thrown from router.
   * Default behavior is to send the `404 Not Found` to client.
   */
  def onNotFound(e: Throwable, ctx: CircumflexContext, chain: FilterChain) = {
    ErrorResponse(404, e.getMessage)(ctx.response)
  }

  /**
   * Instantiates a router that processes current request.
   */
  def doFilter(ctx: CircumflexContext, chain: FilterChain): Unit = {
    log.debug(ctx.request.toString)
    // Set X-Powered-By header
    ctx.response.setHeader("X-Powered-By", "Circumflex v. 0.3")
    // Set character encoding
    ctx.request.setCharacterEncoding("UTF-8")
    try {
      routerClass.getConstructor().newInstance()
      // Request not matched by router
      onNoMatch(ctx, chain)
    } catch {
      case e: InvocationTargetException if e.getCause.isInstanceOf[RouteMatchedException] =>
        // Request matched
        e.getCause.asInstanceOf[RouteMatchedException].response match {
          case Some(response) => response(ctx.response)
          case _ =>
        }
      case e: InvocationTargetException if e.getCause.isInstanceOf[FileNotFoundException] =>
        onNotFound(e, ctx, chain)
      case e =>
        onRouterError(e, ctx, chain)
    }
  }

  /**
   * Called when a filter is instantiated by Servlet Container.
   */
  override def init(cfg: FilterConfig) = {
    log.info("Circumflex v. 0.3")
  }

}
