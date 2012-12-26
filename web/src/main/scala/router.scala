package pro.savant.circumflex
package web

import core._
import util.matching.Regex
import scala.xml.Node

/*!# Routing

Circumflex Web Framework is designed around the _route_ concept. A route is an HTTP
method paired with some matching mechanism and attached block.

Each route defines a `Matcher` which describes the conditions a request must satisfy
in order to be matched. If such conditions are met, an attached block is executed
which yields `RouteResponse` to the client.

Routes are declared right inside the body of `Router` and are executed one-by-one
until first successful match. If no routes match the request, the `404 NOT FOUND` is sent
to the client (unless `onNoMatch` method is overriden in `CircumflexFilter`).

Inside an attached block you can access `MatchResult` object produced by enclosing route.
Match results are stored in `Context`, you can look them up by name.

Take a look at our test sources at [`web/src/test/scala`][tests] to see routers
in action.
*/
class Router {

  def prefix = request.prefix

  implicit def string2response(str: String): RouteResponse =
    new RouteResponse(str)

  implicit def xml2response(xml: Node): RouteResponse = {
    response.contentType("application/xml")
    new RouteResponse("<?xml version=\"1.0\"?>\n" + xml.toString)
  }

  implicit def router2response(router: Router): RouteResponse =
    sendError(404)

  implicit def string2uriMatcher(str: String): UriMatcher =
    new UriMatcher(prefix + str)

  implicit def regex2uriMatcher(regex: Regex): UriMatcher =
    new UriMatcher(new Regex(prefix + regex.toString))

  // Routes
  val get = new Route("get", "head")
  val getOrPost = new Route("get", "post")
  val head = new Route("head")
  val post = new Route("post")
  val put = new Route("put")
  val patch = new Route("patch")
  val delete = new Route("delete")
  val options = new Route("options")
  val any = new Route("*")

  /*! ## Filtering requests

  The `filter` route performs URI-based matching just like all other routes,
  executes the attached block on successful match and continues router execution.

  Unlike endpoint-routes, the `filter` route accepts any kind of block and is
  not required to yield the response.
  */
  val filter = new FilterRoute

  def uri: MatchResult = ctx.getAs[MatchResult]("uri")
      .getOrElse(new MatchResult("uri", "splat" -> request.uri))

  /*!## URI rewriting

  Special `rewrite` route allows you to change URI for currently processed request.
  If route has matched successfully, the URI will be set to the value returned by
  the attached block. After that the matching will be continued.

  This affects `request.uri` method which is used in standard routes.
  Use `request.originalUri` to access original URI after it has been rewritten.

  Here's the example of using URI rewriting:

      rewrite("/a") = "/a/info"

      get("/a") = { ... }   // will never match
      get("/a/info") = { ... }  // will match for both `/a` and `/a/info` requests
  */

  val rewrite = new RewriteRoute

  /*!## Subroutes

  Subroutes represent an easy and powerful concept which allows nesting
  routes inside each other without creating additional routers or repeating
  URI prefixes in matchers.

  Consider the following example:

      class UsersRouter extends Router {

        sub("/users") = {

          get("/?") = "list all users"

          sub("/:userId") = User.get(param("userId")) match {
            case Some(u: User) =>

              // continue matching with prefix "/users/:userId"
              get("/profile") = "Profile of user #" + u.id()

              get("/accounts") = "Accounts of user #" + u.id()
              // ...
            case _ => sendError(404)
          }

        }
      }
  */
  val sub = new SubRoute
}

/*! ## Routing internals

All routes are organized internally with `RoutingContext` which
accepts the contravariant type parameter which specifies the type of block
it accepts.
*/
trait RoutingContext[-T] {

  def matches: Boolean

  protected def dispatch(block: => T)

  def and: RoutingContext[T] = if (matches) this else NopRoute

  def apply(matcher: Matcher): RoutingContext[T] = matcher.apply match {
    case Some(matchResults) if matches =>
      matchResults.foreach(m => ctx.update(m.name, m))
      this
    case _ => NopRoute
  }

  def apply(condition: => Boolean): RoutingContext[T] =
    if (matches && condition) this else NopRoute

  def update(matcher: Matcher, block: => T) {
    apply(matcher).dispatch(block)
  }

  def update(condition: => Boolean, block: => T) {
    apply(condition).dispatch(block)
  }
}

class Route(matchingMethods: String*)
    extends RoutingContext[RouteResponse] {

  val matches = matchingMethods.contains("*") ||
      matchingMethods.contains(request.method)

  protected def dispatch(block: => RouteResponse) {
    val response = block.body
    send(response)
  }
}

class FilterRoute extends RoutingContext[Any] {

  def matches = true

  protected def dispatch(block: => Any) {
    block
  }
}

class RewriteRoute extends RoutingContext[String] {

  def matches = true

  protected def dispatch(block: => String) {
    val newUri = block
    ctx.update("cx.web.uri", newUri)
  }
}

class SubRoute extends RoutingContext[Any] {

  override def apply(matcher: Matcher) = matcher.matchPrefix match {
    case Some((p, matchResults)) if matches =>
      request.setPrefix(p)
      matchResults.foreach(m => ctx.update(m.name, m))
      this
    case _ => NopRoute
  }

  def matches = true

  protected def dispatch(block: => Any) {
    block
    sendError(404)
  }
}

object NopRoute extends RoutingContext[Any] {

  protected def dispatch(block: => Any) {}

  def matches = false
}

case class RouteResponse(body: String)
