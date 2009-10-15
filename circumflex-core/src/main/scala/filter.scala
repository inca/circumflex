package circumflex.core

import java.io.File
import java.lang.reflect.{InvocationTargetException, Constructor}
import java.util.Enumeration
import javax.servlet._
import http.{HttpServletResponse, HttpServletRequest}
import org.slf4j.LoggerFactory
import util.matching.Regex


/**
 * Provides a base class for Circumflex filter implementations.
 */
abstract class AbstractCircumflexFilter extends Filter {

  /**
   * Request attribute key to store RouteContext object.
   */
  val ctxKey = "CircumflexContext"

  /**
   * Place your application initialization code here.
   * Does nothing by deault.
   */
  def init(filterConfig: FilterConfig) = {}

  /**
   * Place your application shutdown code here.
   * Does nothing by deault.
   */
  def destroy = {}

  /**
   * Determines, if a filter should process the request.
   * Used primarily for static resources, where no processing is required.
   * @param req   the request instance
   * @returns     <b>true</b> if the request should be processed;
   *              <b>false</b> if the processing should be skipped.
   */
  def isProcessed(req: HttpServletRequest): Boolean = !req.getRequestURI.matches("/static/.*")

  /**
   * Instantiates a {@link RouteContext} object, binds it to current request,
   * consults <code>isProcessed</code>, whether the request should be processed
   * and delegates to abstract <code>doFilter(RouteContext, FilterChain)</code> if necessary.
   */
  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain): Unit =
    (req, res) match {
      case (req: HttpServletRequest, res: HttpServletResponse) => {
        // Instantiate a context if it does not yet exist
        // and bind it with current request
        if (req.getAttribute(ctxKey) == null)
          req.setAttribute(ctxKey, new RouteContext(req, res, this, Map()))
        val ctx = req.getAttribute(ctxKey).asInstanceOf[RouteContext]
        // Perform processing
        if (isProcessed(req)) {
          doFilter(ctx, chain)
        } else {
          chain.doFilter(req, res)
        }
      } case _ =>
    }

  def doFilter(ctx: RouteContext, chain: FilterChain): Unit

}

/**
 * Configures Circumflex-based web application and serves it's requests. Web application should provide an
 * implementation of this filter and configure it according to JSR-154 (Java Servlet Specification) via
 * <code>your_webapp_root/WEB-INF/web.xml</code> (also known as Deployment Descriptor).
 * Only <code>routerClass</code> method is required for implementation, all other methods have their sensible
 * defaults in place -- you are welcome to override them if your webapp needs it.
 */
abstract class CircumflexFilter[T <: RequestRouter] extends AbstractCircumflexFilter {

  val log = LoggerFactory.getLogger("circumflex.core.filter")

  def routerClass: Class[T]

  def onNoMatch(ctx: RouteContext, chain: FilterChain) =
    chain.doFilter(ctx.request, ctx.response)

  def onRouterError(e: Throwable, ctx: RouteContext, chain: FilterChain) = {
    log.error("Controller threw an exception, see stack trace for details.", e)
    ErrorResponse(ctx, 500, e.getMessage)(ctx.response)
  }

  def doFilter(ctx: RouteContext, chain: FilterChain): Unit = {
    // Set X-Powered-By header
    ctx.response.setHeader("X-Powered-By", "Circumflex v.0.2")
    // Set character encoding
    ctx.request.setCharacterEncoding("UTF-8")
    try {
      routerClass.getConstructor(classOf[RouteContext]).newInstance(ctx)
      // Request not matched by router
      onNoMatch(ctx, chain)
    } catch {
      case e: InvocationTargetException if e.getCause.isInstanceOf[RouteMatchedException] => {
        // Request matched
        e.getCause.asInstanceOf[RouteMatchedException].response match {
          case Some(response) => response(ctx.response)
          case _ =>
        }
      } case e => {
        onRouterError(e, ctx, chain)
      }
    }
  }
}