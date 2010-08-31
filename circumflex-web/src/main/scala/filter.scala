package ru.circumflex.web

import java.lang.reflect.InvocationTargetException
import javax.servlet._
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import java.io._
import java.net.URLDecoder
import ru.circumflex.core._

/*! Circumflex Filter

`CircumflexFilter` is an entry point of your web application. It handles
context lifecycle (initializes context before the request is processed and
finalizes context after the response is sent), serves [static files](#static)
and executes main request router.

To setup your web application place following snippet into your `WEB-INF/web.xml`:

    <filter>
      <filter-name>Circumflex Filter</filter-name>
      <filter-class>ru.circumflex.web.CircumflexFilter</filter-class>
    </filter>

    <filter-mapping>
      <filter-name>Circumflex Filter</filter-name>
      <url-pattern>*</url-pattern>
    </filter-mapping>

You can also include `<dispatcher>REQUEST</dispatcher>`, `<dispatcher>FORWARD</dispatcher>`,
`<dispatcher>INCLUDE</dispatcher>` and `<dispatcher>ERROR</dispatcher>` under `filter-mapping`
if your application requires so.

The filter configuration is saved into the `cx.filterConfig` configuration parameter and
is available throughout your configuration via the `filterConfig` method of the
`ru.circumflex.web` package.
*/

/**
 * Serves as an entry point of Circumflex Web Application.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0/circumflex-web/filter.scala">filter.scala</a>.
 */
class CircumflexFilter extends Filter {

  /**
   * Place your application initialization code here.
   * By default it saves filter configuration into the `cx.filterConfig` configuration
   * parameter so that it could be accessed later.
   */
  def init(filterConfig: FilterConfig) = {
    WEB_LOG.info("Circumflex 2.0")
    cx("cx.filterConfig") = filterConfig
  }

  /**
   * Place your application shutdown code here.
   * Does nothing by default.
   */
  def destroy = {}

  /*## Serving static  {#static}

  Static files are images, stylesheets, javascripts and all other application assets
  which do not require special processing and can be served to clients "as is".

  By default static files are served from `/public` location of your webapp root,
  but you can specify different location by setting the `cx.public` configuration
  parameter.
  */
  def serveStatic(req: HttpServletRequest, res: HttpServletResponse): Boolean = {
    if (req.getMethod.equalsIgnoreCase("get") || req.getMethod.equalsIgnoreCase("head")) {
      val uri = URLDecoder.decode(
        cx.getOrElse("cx.public", "/public").toString + req.getRequestURI, "UTF-8")
      val resource = new File(filterConfig.getServletContext.getRealPath(uri))
      if (resource.isFile) {
        req.getRequestDispatcher(uri).forward(req, res)
        return true
      }
    }
    return false
  }

  /*## Main Lifecycle {#lifecycle}

  The lifecycle of `CircumflexFilter` involves following actions:

    1. try to serve static context and immediately exit on success;
    2. initialize `Context` and fill it with following variables:

      * `cx.request` will hold current `HttpRequest`;
      * `cx.response` will hold current `HttpResponse`;
      * `cx.filterChain` will hold current `FilterChain`;

    3. the main router is instantiated (it's class should be specified via the
    `cx.router` configuration parameter;
    4. depending on the result of router's execution, either the response or
    the error is flushed to the client;
    5. the `Context` is destroyed.

  */
  def doFilter(req: ServletRequest,
               res: ServletResponse,
               chain: FilterChain): Unit = (req, res) match {
    case (req: HttpServletRequest, res: HttpServletResponse) =>
      // try to serve static first
      if (serveStatic(req, res)) return
      // initialize context
      Context.init()
      ctx("cx.request") = new HttpRequest(req)
      ctx("cx.response") = new HttpResponse(res)
      ctx("cx.filterChain") = chain
      try {
        WEB_LOG.trace(req)
        // execute main router
        try {
          cx.instantiate("cx.router") // ResponseSentException must be thrown
          onNoMatch()
        } catch {
          case e: InvocationTargetException => e.getCause match {
            case e: ResponseSentException => throw e
            case e: FileNotFoundException => onNotFound(e)
            case e => onRouterError(e)
          }
        }
      } catch {
        case e: ResponseSentException => WEB_LOG.trace(res)
      } finally {
        // destroy context
        Context.destroy()
      }
    case _ =>
  }

  /*!## Callbacks

  `CircumflexFilter` allows you to override following callbacks:

    * `onNoMatch` is executed if no routes match current request;
    * `onNotFound` is executed if a `FileNotFoundException` is thrown from a router;
    * `onRouterError` is executed if a general exception is thrown from a router;
  */

  def onNoMatch(): Unit = {
    WEB_LOG.debug("No routes matched: " + request)
    sendError(404)
  }

  def onRouterError(e: Throwable): Unit = {
    WEB_LOG.error("Router threw an exception, see stack trace for details.", e)
    sendError(500)
  }

  def onNotFound(e: Throwable): Unit = {
    WEB_LOG.debug("Resource not found, see stack trace for details.", e)
    sendError(404)
  }

}
