package circumflex.core

import _root_.freemarker.template.Template
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import scala.util.matching.Regex

case class RouteMatchedException(val response: Option[HttpResponse]) extends Exception

class RequestRouter(val request: HttpServletRequest, val response: HttpServletResponse, val config: Config) {

  implicit def textResponse(text: String) = TextResponse(ctx, text) 

  var ctx: RequestContext = new RequestContext(request);
  val get = new RequestDispatcher("get")
  val getOrPost = new RequestDispatcher("get", "post")
  val getOrHead = new RequestDispatcher("get", "head")
  val post = new RequestDispatcher("post")
  val put = new RequestDispatcher("put")
  val delete = new RequestDispatcher("delete")
  val head = new RequestDispatcher("head")

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


  class RequestDispatcher(val matchingMethods: String*) {

    protected def dispatch(response: =>HttpResponse, matchers: RequestMatcher*) =
      matchingMethods.find(request.getMethod.equalsIgnoreCase(_)) match {
        case Some(_) => matchRequest(Some(new RequestContext(request)), matchers.toList) match {
          case Some(c: RequestContext) => {
            ctx = c;
            throw RouteMatchedException(Some(response))
          } case _ =>
        } case _ =>
      }

    private def matchRequest(context: Option[RequestContext], matchers: List[RequestMatcher]): Option[RequestContext] =
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

}
