package ru.circumflex.web

import ru.circumflex.core._
import util.matching.Regex
import xml.Node

/*!# Routing

Circumflex Web Framework is designed around the _route_ concept. A route is an HTTP
method paired with some matching mechanism and attached block.

Each route defines a `Matcher` which describes the conditions a request must satisfy
in order to be matched. If such conditions are met, an attached block is executed
which yields `RouteResponse` to the client.

Routes are declared right inside the body of `RequestRouter` and are executed one-by-one
until first successful match. If no routes match the request, the `404 NOT FOUND` is sent
to the client (unless `onNoMatch` method is overriden in `CircumflexFilter`).

Inside an attached block you can access `MatchResult` object produced by enclosing route.
Match results are stored in `Context`, you can look them up by name.

Take a look at our test sources at [`circumflex-web/src/test/scala`][tests] to see routers
in action.

   [tests]: http://github.com/inca/circumflex/tree/master/circumflex-web/src/test/scala/
*/

/**
 * Performs request routing for an application.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0.2/circumflex-web/router.scala">router.scala</a>.
 */
class RequestRouter(var prefix: String = "") {

  implicit def string2response(str: String): RouteResponse =
    new RouteResponse(str)
  implicit def xml2response(xml: Node): RouteResponse = {
    response.contentType("application/xml")
    new RouteResponse("<?xml version=\"1.0\"?>\n" + xml.toString)
  }

  implicit def router2response(router: RequestRouter): RouteResponse =
    sendError(404)

  implicit def string2uriMatcher(str: String): RegexMatcher =
    new RegexMatcher("uri", request.uri, servletContext.getContextPath + prefix + str)
  implicit def regex2uriMatcher(regex: Regex): RegexMatcher =
    new RegexMatcher("uri", request.uri,
      new Regex(servletContext.getContextPath + prefix + regex.toString))

  // Routes
  val get = new Route("get")
  val head = new Route("head")
  val getOrPost = new Route("get", "post")
  val getOrHead = new Route("get", "head")
  val post = new Route("post")
  val put = new Route("put")
  val patch = new Route("patch")
  val delete = new Route("delete")
  val options = new Route("options")
  val any = new Route("get", "post", "put", "patch" , "delete", "head", "options")

  // Filter
  val filter = new FilterRoute

  // Shortcuts
  def error(statusCode: Int = 400, message: String = "No message available."): Nothing =
    sendError(statusCode, message)
  def redirect(url: String, flashes: (String, Any)*): Nothing =
    sendRedirect(url, flashes: _*)
  def uri: MatchResult = ctx.get("uri") match {
    case Some(m: MatchResult) => m
    case None => new MatchResult("uri", "splat" -> request.uri)
  }

  /*!## Subroutes

  Subroutes represent an easy and powerful concept which allows nesting
  routes inside each other without creating additional routers.

  Consider the following example:

      class UsersRouter extends RequestRouter("/users") {
        get("/") = "list all users"
        any("/:userId/+") = User.get(param("userId")) match {
          case Some(u: User) => subroute("/" + u.id()) {
            // continue matching with prefix "/users/:userId"
            get("/profile") = "Profile of user #" + u.id()
            get("/accounts") = "Accounts of user #" + u.id()
            // ...
          }
          case _ => sendError(404)
        }
      }

  */
  def subroute(newPrefix: String)(block: => RouteResponse): RouteResponse = {
    prefix += newPrefix
    block
  }

}

trait RoutingContext[-T] {
  def matches: Boolean
  protected def dispatch(block: => T): Unit
  def and: RoutingContext[T] = if (matches) this else NopRoute
  def apply(matcher: Matcher): RoutingContext[T] = matcher.apply() match {
    case Some(matchResults) if matches =>
      matchResults.foreach(m => ctx.update(m.name, m))
      return this
    case _ => return NopRoute
  }
  def apply(condition: => Boolean): RoutingContext[T] =
    if (matches && condition) this else NopRoute
  def update(matcher: Matcher, block: => T): Unit =
    apply(matcher).dispatch(block)
  def update(condition: => Boolean, block: => T): Unit =
    apply(condition).dispatch(block)
}

class Route(matchingMethods: String*) extends RoutingContext[RouteResponse] {
  val matches = matchingMethods.contains(request.method)
  protected def dispatch(block: => RouteResponse): Unit = {
    val response = block.body
    send(response)
  }
}

class FilterRoute extends RoutingContext[Unit] {
  def matches = true
  protected def dispatch(block: => Unit) = block
}

object NopRoute extends RoutingContext[Any] {
  protected def dispatch(block: => Any): Unit = {}
  def matches = false
}

/**
 * @see RequestRouter
 */
case class RouteResponse(val body: String)