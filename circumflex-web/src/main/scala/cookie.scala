package ru.circumflex.web

import javax.servlet.http.Cookie

/*!# HTTP Cookies

HTTP is stateless protocol. [RFC2965](http://www.faqs.org/rfcs/rfc2965.html) describes
a way for a web application to send state information to the user agent and for the user
agent to return the state information to the origin web application. This state information
is often refered to as *cookie*.

Circumflex Web Framework helps you set response cookies and access request cookies
throughout your application using case class `HttpCookie`. It is mutable and dead simple.
*/

/**
 * Provides convenient mutable wrapper to work with HTTP cookies.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0/circumflex-web/cookie.scala">cookie.scala</a>.
 */
case class HttpCookie(var name: String,
                      var value: String,
                      var domain: String = null,
                      var path: String = null,
                      var comment: String = null,
                      var secure: Boolean = false,
                      var maxAge: Int = -1) {
/*! You can convert `HttpCookie` back to `javax.servlet.Cookie` using `convert` method. */
  def convert: Cookie = {
    val c = new Cookie(name, value)
    if (domain != null) c.setDomain(domain)
    if (path != null) c.setPath(path)
    if (comment != null) c.setComment(comment)
    c.setSecure(secure)
    c.setMaxAge(maxAge)
    return c
  }
  override def toString = name + " = " + value
}

/*! Depending on your application needs you can obtain an instance of `HttpCookie` by
supplying `javax.servlet.Cookie` as an argument to `apply` method of `HttpCookie` singleton:

    HttpCookie(rawCookie)
*/

/**
 * Converts the `javax.serlvet.Cookie` into `HttpCookie`.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0/circumflex-web/cookie.scala">cookie.scala</a>.
 */
object HttpCookie {
  def apply(cookie: Cookie): HttpCookie =
    new HttpCookie(
      cookie.getName,
      cookie.getValue,
      cookie.getDomain,
      cookie.getPath,
      cookie.getComment,
      cookie.getSecure,
      cookie.getMaxAge)
}