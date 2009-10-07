package circumflex.core

import _root_.freemarker.template.Template
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import scala.util.matching.Regex

case class RouteMatchedException(val response: Option[HttpResponse]) extends Exception

class RequestRouter(val request: HttpServletRequest,
                    val response: HttpServletResponse,
                    val config: Config,
                    var ctx: RouteContext) {

  implicit def textResponse(text: String) = TextResponse(ctx, text)

  val get = new RequestDispatcher("get")
  val getOrPost = new RequestDispatcher("get", "post")
  val getOrHead = new RequestDispatcher("get", "head")
  val post = new RequestDispatcher("post")
  val put = new RequestDispatcher("put")
  val delete = new RequestDispatcher("delete")
  val head = new RequestDispatcher("head")

  val header = new HeadersHelper

  def rewrite(target: String): Nothing = {
    request.getRequestDispatcher(target).forward(request, response)
    throw RouteMatchedException(None)
  }

  def headers(crit: (String,String)*) = new HeadersPatternMatcher(crit : _*)
  def headers(crit: (String,Regex)*) = new HeadersRegexMatcher(crit : _*)

  def ftl(templateName:String) = {
    val template = config.freemarkerConf.getTemplate(templateName);
    new FreemarkerResponse(ctx, template)
  }

  def param(key: String): Option[String] = ctx.params.get(key) match {
    case Some(value: String) => Some(value)
    case _ => {
      val value = request.getParameter(key)
      if (value == null) None
      else Some(value)
    }
  }

  def method = param("_method").getOrElse(request.getMethod)

  def error(errorCode: Int, message: String) = ErrorResponse(ctx, errorCode, message)
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

    protected def dispatch(response: =>HttpResponse, matchers: RequestMatcher*) =
      matchingMethods.find(method.equalsIgnoreCase(_)) match {
        case Some(_) => matchRequest(Some(new RouteContext(request)), matchers.toList) match {
          case Some(c: RouteContext) => {
            ctx ++= c;
            throw RouteMatchedException(Some(response))
          } case _ =>
        } case _ =>
      }

    private def matchRequest(context: Option[RouteContext],
                             matchers: List[RequestMatcher]): Option[RouteContext] =
      context match {
        case Some(c) if matchers == Nil => Some(c)
        case Some(c) => matchRequest(matchers.head(c), matchers.tail)
        case _ => None
      }

    def update(uriRegex: Regex, response: =>HttpResponse): Unit =
      dispatch(response, new UriRegexMatcher(uriRegex))

    def update(uriPattern: String, response: =>HttpResponse): Unit =
      dispatch(response, new UriPatternMatcher(uriPattern))

    def update(uriRegex: Regex, matcher1: RequestMatcher, response: =>HttpResponse): Unit =
      dispatch(response, new UriRegexMatcher(uriRegex), matcher1)

    def update(uriPattern: String, matcher1: RequestMatcher, response: =>HttpResponse): Unit =
      dispatch(response, new UriPatternMatcher(uriPattern), matcher1)
  }


  class HeadersHelper {

    def apply(name: String): Option[String] = {
      val value = request.getHeader(name)
      if (value == null) None
      else Some(value)
    }

    def update(name: String, value: String) = ctx.stringHeaders += name -> value

    def update(name: String, value: Long) = ctx.dateHeaders += name -> value

    def update(name: String, value: java.util.Date) = ctx.dateHeaders += name -> value.getTime

  }

}
