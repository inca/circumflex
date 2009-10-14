package circumflex.core

import _root_.freemarker.template.Template
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}

case class RouteMatchedException(val response: Option[HttpResponse]) extends Exception

class RequestRouter(var ctx: RouteContext) {

  implicit def textToResponse(text: String): HttpResponse = TextResponse(ctx, text)

  val get = new RequestDispatcher("get")
  val getOrPost = new RequestDispatcher("get", "post")
  val getOrHead = new RequestDispatcher("get", "head")
  val post = new RequestDispatcher("post")
  val put = new RequestDispatcher("put")
  val delete = new RequestDispatcher("delete")
  val head = new RequestDispatcher("head")

  val header = new HeadersHelper

  def isXhr = header("X-Requested-With") match {
    case Some("XMLHttpRequest") => true
    case _ => false
  }

  def rewrite(target: String): Nothing = {
    ctx.request.getRequestDispatcher(target).forward(ctx.request, ctx.response)
    throw RouteMatchedException(None)
  }

  def headers(crit: (String, String)*) = new HeadersRegexMatcher(crit : _*)

  def ftl(templateName:String) = {
    val template = ctx.config.freemarkerConf.getTemplate(templateName);
    new FreemarkerResponse(ctx, template)
  }

  def param(key: String) = ctx.stringParam(key)

  def method = param("_method").getOrElse(ctx.request.getMethod)

  def error(errorCode: Int, message: String) = ErrorResponse(ctx, errorCode, message)
  def error(errorCode: Int) = ErrorResponse(ctx, errorCode, "")

  def redirect(location: String) = RedirectResponse(ctx, location)

  def done: HttpResponse = done(200)

  def done(statusCode: Int): HttpResponse = {
    ctx.statusCode = statusCode
    EmptyResponse(ctx)
  }

  def requireParams(names: String*) = names.toList.foreach(name => {
    if (param(name) == None)
      throw new RouteMatchedException(Some(error(400, "Missing " + name + " parameter.")))
  })

  class RequestDispatcher(val matchingMethods: String*) {

    protected def dispatch(response: =>HttpResponse, matchers: RequestMatcher*): Unit =
      matchingMethods.find(method.equalsIgnoreCase(_)) match {
        case Some(_) => {
          var params = Map[String, String]()
          matchers.toList.foreach(rm => rm(ctx.request) match {
            case None => return
            case Some(p) => params ++= p
          })
          // All matchers succeeded
          ctx ++= params
          throw RouteMatchedException(Some(response))
        } case _ =>
      }

    def update(uriRegex: String, response: =>HttpResponse): Unit =
      dispatch(response, new UriRegexMatcher(uriRegex))

    def update(uriRegex: String, matcher1: RequestMatcher, response: =>HttpResponse): Unit =
      dispatch(response, new UriRegexMatcher(uriRegex), matcher1)
  }

  class HeadersHelper {

    def apply(name: String): Option[String] = {
      val value = ctx.request.getHeader(name)
      if (value == null) None
      else Some(value)
    }

    def update(name: String, value: String) = ctx.stringHeaders += name -> value

    def update(name: String, value: Long) = ctx.dateHeaders += name -> value

    def update(name: String, value: java.util.Date) = ctx.dateHeaders += name -> value.getTime

  }

}
