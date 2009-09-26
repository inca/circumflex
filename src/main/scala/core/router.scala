package circumflex.core

import _root_.freemarker.template.Template
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import scala.util.matching.Regex

case class RouteMatchedException(val request: HttpRequest, val response: Option[HttpResponse]) extends Exception

class RequestRouter(val request: HttpServletRequest, val response: HttpServletResponse, val config: Config) {

  implicit def textToResponse(text: String): HttpResponse = TextResponse(text)

  protected var req: HttpRequest = null;

  val get = new RequestDispatcher("get")
  val getOrPost = new RequestDispatcher("get", "post")
  val getOrHead = new RequestDispatcher("get", "head")
  val post = new RequestDispatcher("post")
  val put = new RequestDispatcher("put")
  val delete = new RequestDispatcher("delete")
  val head = new RequestDispatcher("head")

  val rewrite = new RewriteHelper

  def headers(crit: (String,String)*) = new HeadersPatternMatcher(crit : _*)
  def headers(crit: (String,Regex)*) = new HeadersRegexMatcher(crit : _*)

  def ftl(templateName:String) = {
    val template = config.freemarkerConf.getTemplate(templateName);
    var context = Map[String,Any]()
    context ++= req.params
    new FreemarkerTemplateResponse(template, context)
  }

  class RequestDispatcher(val matchingMethods: String*) {
    protected def dispatch(response: =>HttpResponse, matchers: RequestMatcher*) =
      matchingMethods.find(request.getMethod.equalsIgnoreCase(_)) match {
        case Some(_) => matchRequest(Some(new HttpRequest(request)), matchers.toList) match {
          case Some(r: HttpRequest) => {
            req = r;
            throw RouteMatchedException(req, Some(response))
          } case _ =>
        } case _ =>
      }

    private def matchRequest(request: Option[HttpRequest], matchers: List[RequestMatcher]): Option[HttpRequest] =
     request match {
       case Some(req) if matchers == Nil => Some(req)
       case Some(req) => matchRequest(matchers.head(req), matchers.tail)
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

  class RewriteHelper {
    def apply(target: => String) = {
      request.getRequestDispatcher(target).forward(request, response)
      throw RouteMatchedException(req, None)
    }
  }

}
