package ru.circumflex.core

import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import java.net.URLDecoder
import java.util.ResourceBundle
import java.lang.String

class CircumflexContext(val request: HttpServletRequest,
                        val response: HttpServletResponse)
    extends HashModel { c =>

  /**
   * A helper for looking up the parameters that come from matching.
   */
  object param extends HashModel {
    def get(key: String): Option[String] = c.get(key) match {
      case Some(value: String) => Some(value)
      case _ => _params.values.flatMap(o => o match {
        case m: Match => m.params
        case _ => Nil
      }).find(p => p._1 == key) match {
        case Some(pair: (String, String)) => Some(pair._2)
        case _ => None
      }
    }
    override def apply(key: String): String = getOrElse(key, "")
  }

  /**
   * A helper for getting and setting response headers in a DSL-like way.
   */
  object header extends HashModel {
    def get(name: String): Option[String] = request.getHeader(name)
    def update(name: String, value: String): Unit = response.setHeader(name, value)
    def update(name: String, value: Long): Unit = response.setDateHeader(name, value)
    def update(name: String, value: java.util.Date): Unit = update(name, value.getTime)
  }

  /**
   * A helper for getting and setting session-scope attributes.
   */
  object session extends HashModel {
    def get(name: String): Option[Any] = request.getSession.getAttribute(name)
    def update(name: String, value: Any) = request.getSession.setAttribute(name, value)
  }

  /**
   * A helper for setting flashes. Flashes provide a way to pass temporary objects between requests.
   */
  object flash extends HashModel {
    val SESSION_KEY = "cx.flash"
    def get(key: String): Option[Any] = {
      val flashMap = session.getOrElse(SESSION_KEY, MutableMap[String, Any]())
      flashMap.get(key) map { value => {
        session(SESSION_KEY) = flashMap - key
        value
      }}
    }
    def update(key: String, value: Any) = {
      val flashMap = session.getOrElse(SESSION_KEY, MutableMap[String, Any]())
      session(SESSION_KEY) = flashMap + (key -> value)
    }
  }

  /**
   * A helper for getting and setting HTTP cookies.
   */
  object cookie extends HashModel {
    lazy val all: Seq[HttpCookie] = if (ctx.request.getCookies == null) Nil
    else ctx.request.getCookies.map(c => HttpCookie.convert(c))
    def get(key: String): Option[HttpCookie] = all.find(c => c.name == key)
    def set(name: String,
            value: String,
            domain: String = null,
            path: String = null,
            comment: String = null,
            secure: Boolean = false,
            maxAge: Int = -1): Unit =
      set(new HttpCookie(name, value, domain, path, comment, secure, maxAge))
    def set(cookie: HttpCookie): Unit = ctx.response.addCookie(cookie.convert)
  }

  /**
   * A helper for retrieving messages from `Messages.properties` resource bundle.
   */
  object messages extends Messages(Circumflex.msgBundle, locale)

  // ### Request commons

  protected var _contentType: String = null
  def contentType: Option[String] = _contentType
  def contentType_=(value: String) = { _contentType = value }
  var statusCode: Int = 200
  def method: String = getOrElse('_method, request.getMethod)
  def uri = URLDecoder.decode(request.getRequestURI, "UTF-8")
  def locale = request.getLocale

  // ### Parameters

  private val _params = MutableMap[String, Any](
    "header" -> header,
    "session" -> session,
    "flash" -> flash,
    "msg" -> messages,
    "cookie" -> cookie,
    "ctx" -> this
    )
  def get(key: String): Option[Any] = _params.get(key) match {
    case Some(value) if (value != null) => value
    case _ => request.getParameter(key)
  }
  def update(key: String, value: Any) { _params += key -> value }
  def +=(pair: (String, Any)) { _params += pair }

}

object CircumflexContext {
  private val threadLocalContext = new ThreadLocal[CircumflexContext]
  def get = threadLocalContext.get
  def live_?() = get != null
  def init(req: HttpServletRequest, res: HttpServletResponse) =
    if (!live_?) threadLocalContext.set(new CircumflexContext(req, res))
  def destroy() = threadLocalContext.set(null)
}

class ParamHelper(val key: String) {
  def :=(value: Any): Unit = { ctx(key) = value }
}
