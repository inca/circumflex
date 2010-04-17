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

  def allMatches(src: String, groupName: Int => String): Option[Map[String, String]] = {
    val m = regex.pattern.matcher(src)
    if (m.matches) {
      val matches = for (i <- 1 to m.groupCount) yield groupName(i) -> m.group(i)
      Some(Map(matches: _*))
    } else None
  }
}

object RichRegex {
  implicit def regex2RichRegex(regex: Regex) = new RichRegex(regex)
}