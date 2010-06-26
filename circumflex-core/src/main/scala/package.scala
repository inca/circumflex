package ru.circumflex

import ru.circumflex.core._
import org.slf4j.LoggerFactory

package object core {

  implicit def symbol2string(sym: Symbol): String = sym.name
  @inline implicit def any2option[A](a: A): Option[A] = if (a == null) None else Some(a)
  implicit def string2paramHelper(str: String): ParamHelper = new ParamHelper(str)
  implicit def symbol2paramHelper(sym: Symbol): ParamHelper = new ParamHelper(sym.name)

  type MutableMap[A, B] = collection.mutable.Map[A, B]
  val MutableMap  = collection.mutable.Map
  
  val cxLog = LoggerFactory.getLogger("ru.circumflex.core")
  def ctx = CircumflexContext.get

  val Accept = new HeaderMatcherHelper("Accept")
  val AcceptCharset = new HeaderMatcherHelper("Accept-Charset")
  val AcceptEncoding = new HeaderMatcherHelper("Accept-Encoding")
  val AcceptLanguage = new HeaderMatcherHelper("Accept-Language")
  val AcceptRanges = new HeaderMatcherHelper("Accept-Ranges")
  val Authorization = new HeaderMatcherHelper("Authorization")
  val CacheControl = new HeaderMatcherHelper("Cache-Control")
  val ContentLength = new HeaderMatcherHelper("Content-Length")
  val ContentType = new HeaderMatcherHelper("Content-Type")
  val HeaderDate = new HeaderMatcherHelper("Date")
  val Expect = new HeaderMatcherHelper("Expect")
  val Host = new HeaderMatcherHelper("Host")
  val IfMatch = new HeaderMatcherHelper("If-Match")
  val IfModifiedSince = new HeaderMatcherHelper("If-Modified-Since")
  val IfNoneMatch = new HeaderMatcherHelper("If-None-Match")
  val IfRange = new HeaderMatcherHelper("If-Range")
  val IfUnmodifiedSince = new HeaderMatcherHelper("If-Unmodified-Since")
  val MaxForwards = new HeaderMatcherHelper("Max-Forwards")
  val Pragma = new HeaderMatcherHelper("Pragma")
  val ProxyAuthorization = new HeaderMatcherHelper("Proxy-Authorization")
  val Referer = new HeaderMatcherHelper("Referer")
  val Upgrade = new HeaderMatcherHelper("Upgrade")
  val UserAgent = new HeaderMatcherHelper("User-Agent")
  val Via = new HeaderMatcherHelper("Via")

}