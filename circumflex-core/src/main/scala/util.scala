package ru.circumflex.core

import collection.mutable.{HashMap, ListBuffer}
import java.io.File
import util.matching.Regex

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
abstract class NginxXSendFileHeader extends XSendFileHeader {
  def name = "X-Accel-Redirect"
}

/**
 * Rich wrapper for class Regex
 */
class RichRegex(regex: Regex) {

  // TODO: In Scala 2.8 use replaceAllIn
  def replaceAllInF(source: String)(replacer: String => String): String =
    regex.findAllIn(source).matchData
         .map(m => m -> replacer(m.group(0)))
         .foldRight(source) {
           case ((m, replacement), result) =>
             result.substring(0, m.start) + replacement + result.substring(m.end)
         }

}

object Convertions {

  implicit def regex2RichRegex(regex: Regex): RichRegex = new RichRegex(regex)
  implicit def symbol2String(sym: Symbol): String = sym.name

  implicit def string2StringMatcher(str: String): StringMatcher = new SimplifiedMatcher(str)
  implicit def regex2StringMatcher(regex: Regex): StringMatcher = new RegexMatcher(regex)
  implicit def pair2String_StringMatcher[A <% String, B <% StringMatcher]
    (p: (A, B)): (String, StringMatcher) = (p._1, p._2)

}