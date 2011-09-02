package ru.circumflex
package web
import core._
import util.matching.Regex
import xml.Node

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

Take a look at our test sources at [`circumflex-web/src/test/scala`][tests] to see routers
in action.

## A bit on trailing slashes

If you write lots of RESTful code, you probably come across following routing paradigm:

    any("/items/:itemId/&#42;") = Items.get(param("itemId")) map { i =>
      subroute("/items/" + i.id) {
        get("/?") = { ... }
      }
    }

With code like this you would expect `/items/6` to hit the inner route,
but it does not satisfy the outer route. In earlier versions you would have
to forward the route `/items/:itemId` to enforce trailing slash inside subroutes.
Since Circumflex 2.1 we made this a bit easier to you:
we simply do not take trailing slash into consideration when matching against the
pattern ending with `/&#42;` (a slash and an asterisk). Note that this only
affects string patterns, regex-patterns are processed normally. This also does not
affect patterns ending with `/+`: in this case trailing slash is required.

   [tests]: http://github.com/inca/circumflex/tree/master/circumflex-web/src/test/scala/
*/
class Router(var prefix: String = "") {

  implicit def string2response(str: String): RouteResponse =
    new RouteResponse(str)
  implicit def xml2response(xml: Node): RouteResponse = {
    response.contentType("application/xml")
    new RouteResponse("<?xml version=\"1.0\"?>\n" + xml.toString)
  }

  implicit def router2response(router: Router): RouteResponse =
    sendError(404)

  implicit def string2uriMatcher(str: String): RegexMatcher = {
    var _uri = request.uri
    var pattern = servletContext.getContextPath + prefix + str
    if (str.endsWith("/*")) {
      _uri += "/"
      pattern += "/?"
    }
    new RegexMatcher("uri", _uri, pattern)
  }
  implicit def regex2uriMatcher(regex: Regex): RegexMatcher =
    new RegexMatcher("uri", request.uri,
      new Regex(servletContext.getContextPath + prefix + regex.toString))

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
  routes inside each other without creating additional routers.

  Consider the following example:

      class UsersRouter extends Router("/users") {
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

  When entering `subroute`, specified `newPrefix` is appended to current prefix
  and specified `block` gets executed. All routes inside this block will be matched
  with respect to this new prefix. If no routes match inside specified `block`,
  `404 NOT FOUND` is sent.
  */
  def subroute(newPrefix: String)(block: => Unit): Nothing = {
    prefix += newPrefix
    block
    sendError(404)
  }

}

trait RoutingContext[-T] {
  def matches: Boolean
  protected def dispatch(block: => T)
  def and: RoutingContext[T] = if (matches) this else NopRoute
  def apply(matcher: Matcher): RoutingContext[T] = matcher.apply() match {
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

class Route(matchingMethods: String*) extends RoutingContext[RouteResponse] {
  val matches = matchingMethods.contains("*") || matchingMethods.contains(request.method)
  protected def dispatch(block: => RouteResponse) {
    val response = block.body
    send(response)
  }
}

class FilterRoute extends RoutingContext[Unit] {
  def matches = true
  protected def dispatch(block: => Unit) {
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

object NopRoute extends RoutingContext[Any] {
  protected def dispatch(block: => Any) {}
  def matches = false
}

case class RouteResponse(body: String)
