package pro.savant.circumflex
package web

import java.lang.reflect.InvocationTargetException
import javax.servlet._
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import java.io._
import util.control.ControlThrowable
import core._

/*!# Circumflex Filter

`CircumflexFilter` is an entry point of your web application. It handles
context lifecycle (initializes context before the request is processed and
finalizes context after the response is sent), serves static files
and executes main request router.

To setup your web application place following snippet into your `WEB-INF/web.xml`:

``` {.xml}
<filter>
  <filter-name>Circumflex Filter</filter-name>
  <filter-class>pro.savant.circumflex.web.CircumflexFilter</filter-class>
</filter>

<filter-mapping>
  <filter-name>Circumflex Filter</filter-name>
  <url-pattern>*</url-pattern>
</filter-mapping>
```

You can also include `<dispatcher>REQUEST</dispatcher>`, `<dispatcher>FORWARD</dispatcher>`,
`<dispatcher>INCLUDE</dispatcher>` and `<dispatcher>ERROR</dispatcher>` under `filter-mapping`
if your application requires so (for example, include `ERROR` dispatcher if you wish to
serve error pages with Circumflex; beware of infinite loops, however).

The filter configuration is saved into the `cx.filterConfig` configuration parameter and
is available throughout your configuration via the `filterConfig` method of the
`pro.savant.circumflex.web` package.
*/
class CircumflexFilter extends Filter {

  def init(filterConfig: FilterConfig) {
    WEB_LOG.info("Circumflex 3.0")
    cx("cx.filterConfig") = filterConfig
  }

  def destroy() {}

  /*!## Serving static

  Static files are images, stylesheets, javascripts and all other application assets
  which do not require special processing and can be served to clients "as is".

  By default static files are served from `/public` location of your webapp root,
  but you can specify different location by setting the `cx.public` configuration
  parameter.
  */
  def serveStatic(req: HttpServletRequest,
                  res: HttpServletResponse,
                  chain: FilterChain): Boolean = {
    if (req.getMethod.equalsIgnoreCase("get") || req.getMethod.equalsIgnoreCase("head")) {
      val uri = req.getRequestURI
      val publicUri = cx.getOrElse("cx.public", "/public").toString
      val contextPath = servletContext.getContextPath
      if (uri.startsWith(contextPath + publicUri) && !uri.endsWith("/")) {
        chain.doFilter(req, res)
        return true
      }
      val relativeUri = uri.substring(contextPath.length)
      val decodedUri = decodeURI(publicUri + relativeUri)
      val path = filterConfig.getServletContext.getRealPath(decodedUri)
      if (path == null)
        return false
      val resource = new File(path)
      if (resource.isFile) {
        req.getRequestDispatcher(decodedUri).forward(req, res)
        return true
      }
    }
    false
  }

  /*!## Main Lifecycle {#lifecycle}

  The lifecycle of `CircumflexFilter` involves following actions:

  1. try to serve static context and immediately exit on success;

  2. initialize `Context` and fill it with following variables:

    * `cx.request` will hold current `HttpRequest`;

    * `cx.response` will hold current `HttpResponse`;

    * `cx.filterChain` will hold current `FilterChain`;

    * `cx.locale` will hold current request locale as obtained from
      raw HTTP request;

    * other variables from `prepareContext`;

  3. the main router is instantiated (it's class should be specified via the
  `cx.router` configuration parameter;

  4. depending on the result of router's execution, either the response or
  the error is flushed to the client;

  5. the `Context` is destroyed.

  */
  def doFilter(req: ServletRequest,
               res: ServletResponse,
               chain: FilterChain) {
    (req, res) match {
      case (req: HttpServletRequest, res: HttpServletResponse) =>
        // try to serve static first
        if (serveStatic(req, res, chain))
          return
        // initialize context
        Context.executeInNew { ctx =>
          ctx("cx.request") = new HttpRequest(req)
          ctx("cx.response") = new HttpResponse(res)
          ctx("cx.filterChain") = chain
          ctx("cx.locale") = req.getLocale
          prepareContext(ctx)
          try {
            WEB_LOG.trace(req)
            // execute main router
            try {
              cx.instantiate("cx.router") // ResponseSentMarker must be thrown
              onNoMatch()
            } catch {
              case e: InvocationTargetException => e.getCause match {
                case ex: ResponseSentMarker => throw ex
                case ex: FileNotFoundException => onNotFound(ex)
                case ex: Exception => onRouterError(ex)
              }
            }
          } catch {
            case e: ResponseSentMarker => WEB_LOG.trace(res)
          }
        }
      case _ =>
    }
  }

  /*! The `prepareContext` method populates current context with various useful
  shortcuts (from `web` package):

  * `param` -- the `param` object;
  * `request` -- the `request` object;
  * `session` -- the `session` object;
  * `cookies` -- the `cookies` object;
  * `headers` -- the `headers` object;
  * `flash` -- the `flash` object;
  * `cfg` -- the `cx` object;
  * `msg` -- the `msg` object.

  If you use custom filter implementation, you are can override this method
  to populate current context with global variables of your application.
  */
  def prepareContext(ctx: Context) {
    'param := param
    'request := request
    'session := session
    'flash := flash
    'cfg := cx
    'msg := msg
  }

  /*!## Callbacks

  `CircumflexFilter` allows you to override following callbacks:

  * `onNoMatch` is executed if no routes match current request;
  * `onNotFound` is executed if a `FileNotFoundException` is thrown from a router;
  * `onRouterError` is executed if a general exception is thrown from a router;

  */
  def onNoMatch() {
    WEB_LOG.debug("No routes matched: " + request)
    sendError(404)
  }

  def onRouterError(e: Throwable) {
    WEB_LOG.error("Router threw an exception, see stack trace for details.", e)
    sendError(500)
  }

  def onNotFound(e: Throwable) {
    WEB_LOG.debug("Resource not found, see stack trace for details.", e)
    sendError(404)
  }

}

/*!## Response Sent Marker

The `ResponseSentMarker` is thrown by response helpers and routes upon successful matching
and are caught by `CircumflexFilter`. They indicate that the response has been processed
successfully (and, possibly, already flushed to the client) and that no additional actions
need to be taken.

In general `ResponseSentMarker` should not be caught by try-catch blocks. It inherits
from Scala's `ControlThrowable` to save some performance.
 */
class ResponseSentMarker extends ControlThrowable