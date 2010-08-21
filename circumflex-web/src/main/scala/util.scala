package ru.circumflex.core

import collection.mutable.{HashMap, ListBuffer}
import java.io.File
import util.matching.Regex
import javax.servlet.http.Cookie

/**
 * Contains utility stuff.
 */
object CircumflexUtil {

  /**
   * Performs a grouping operation on a collection. The result is a map
   * in which keys are obtained via predicate function `predicateFunc` and
   * values are subcollections, every element of which satisfies the predicate.
   */
  def groupBy[K,V](it: Iterable[V], predicateFunc: V => K): collection.Map[K, Seq[V]] = {
    val result = new HashMap[K, ListBuffer[V]] {
      override def default(a: K) = new ListBuffer[V]
    }
    it.foreach(v => {
      val key = predicateFunc(v)
      val buffer: ListBuffer[V] = result(key)
      buffer += v
      result += (key -> buffer)
    })
    result
  }

  /**
   * Now this thingy is very useful and very light:
   * it translates every `ThisKindOfIdentifiers`
   * into `that_kinds_of_identifiers`.
   */
  def camelCaseToUnderscore(arg: String) = arg.replaceAll("(?<!^)([A-Z])","_$1").toLowerCase

  /**
   * Executes specified `block` and reports the time taken.
   */
  def time(block: => Unit): Long = {
    val startTime = System.currentTimeMillis
    block
    System.currentTimeMillis - startTime
  }

}

/**
 * Mutable helper for dealing with HTTP cookies.
 */
case class HttpCookie(var name: String,
                      var value: String,
                      var domain: String = null,
                      var path: String = null,
                      var comment: String = null,
                      var secure: Boolean = false,
                      var maxAge: Int = -1) {
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

object HttpCookie {
  def convert(cookie: Cookie): HttpCookie =
    new HttpCookie(
      cookie.getName,
      cookie.getValue,
      cookie.getDomain,
      cookie.getPath,
      cookie.getComment,
      cookie.getSecure,
      cookie.getMaxAge)
}

/**
 * Represents an HTTP header for `xSendFile` method of `RequestRouter`. X-SendFile feature
 * allows sending files via Web server, thus eliminating all the dirty work from the
 * application code. See the documentation of your Web server to obtain information on
 * how to configure X-SendFile feature.
 */
trait XSendFileHeader {
  def name: String
  def value(f: File): String
}

/**
 * Default `XSendFileHeader` implementation works with most Web browsers,
 * including Apache HTTPD and lighttpd.
 */
object DefaultXSendFileHeader extends XSendFileHeader {
  def name = "X-SendFile"
  def value(f: File): String = f.getAbsolutePath
}

/**
 * Nginx implementation needs to know URI instead of file path, so we leave the
 * implementation details up to you.
 */
trait NginxXSendFileHeader extends XSendFileHeader {
  def name = "X-Accel-Redirect"
}
