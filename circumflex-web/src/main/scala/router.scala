package ru.circumflex.core

import java.io.File
import util.matching.Regex
import xml.Node

// ## Request Router

class RequestRouter(val prefix: String = "") {

  implicit def textToResponse(text: String): HttpResponse = TextResponse(text)
  implicit def xmlToResponse(xml: Node): HttpResponse = TextResponse(xml.toString)
  implicit def requestRouterToResponse(router: RequestRouter): HttpResponse = error(404)

  implicit def string2uriMatcher(str: String): RegexMatcher =
    new RegexMatcher("uri", ctx.uri, prefix + str)

  implicit def regex2uriMatcher(regex: Regex): RegexMatcher =
    new RegexMatcher("uri", ctx.uri, new Regex(prefix + regex.toString))

  /**
   * ## Route
   *
   * Dispatches current request if it passes all matchers.
   * Common matchers are based on HTTP methods, URI and headers.
   */
  class Route(matchingMethods: String*) {

    protected def dispatch(matcher: Matcher, response: =>HttpResponse): Unit =
      matchingMethods.find(ctx.method.equalsIgnoreCase(_)) match {
        case Some(_) =>
          matcher.apply() match {
            case None => return
            case Some(matches: Seq[Matcher]) =>
              matches.foreach(m => ctx += m.name -> m)
              throw new RouteMatchedException(response)
          }
        case _ =>
      }

    /**
     * For syntax "get(...) = response"
     */
    def update(matcher: Matcher, response: =>HttpResponse): Unit =
      dispatch(matcher, response)

  }

  // ### Routes

  val get = new Route("get")
  val getOrPost = new Route("get", "post")
  val getOrHead = new Route("get", "head")
  val post = new Route("post")
  val put = new Route("put")
  val patch = new Route("patch")
  val delete = new Route("delete")
  val head = new Route("head")
  val options = new Route("options")
  val any = new Route("get", "post", "put", "patch" , "delete", "head", "options")

  def uri: MatchResult = ctx.get("uri") match {
    case Some(m: MatchResult) => m
    case None => new MatchResult("uri", "splat" -> ctx.uri)
  }

}
