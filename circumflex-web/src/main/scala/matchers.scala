package ru.circumflex.web

import util.matching.Regex
import scala.collection.immutable.Map
import collection.Iterator

/*!# Matchers

The `Matcher` trait and the `MatchResult` class are the cornerstone of request routing.

Matchers define mechanisms which perform request matching. They yield zero or more
match results on successful match and are used in routes definition.

Match results are subsequently used inside matched route's block.
*/

/*!## Match Results

The results of matching contain information about successful match. The `name` reflects
the name of the `Matcher` which yielded this match result, and the `params` contains
strings captured by `Matcher`. The special name `splat` is assigned to parameters with
unknown name (`+`, `*` or any group, if you use regular expressions).
*/

/**
 * Provides information about successful match.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0.2/circumflex-web/matchers.scala">matchers.scala</a>.
 */
class MatchResult(val name: String,
                  val params: (String, String)*) extends Map[String, String] {
  def +[B1 >: String](kv: (String, B1)): Map[String, B1] = this
  def -(key: String): Map[String, String] = this
  def iterator: Iterator[(String, String)] = params.iterator

  def get(index: Int): Option[String] =
    if (params.indices.contains(index)) Some(params(index)._2)
    else None
  def get(name: String): Option[String] = params.find(_._1 == name) match {
    case Some(param: Pair[String, String]) => Some(param._2)
    case _ => None
  }

  def apply(): String = apply(0)
  def apply(index: Int): String = get(index).getOrElse("")

  def splat: Seq[String] = params.filter(_._1 == "splat").map(_._2).toSeq

  override def default(key: String): String = ""
  override def toString = apply(0)
}

/*! Matchers can be composed together using the `&` method. The `CompositeMatcher` will
only yield match results if all it's matchers succeed.*/

/**
 * Provides matching mechanism to routes.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0.2/circumflex-web/matchers.scala">matchers.scala</a>.
 */
trait Matcher {
  def apply(): Option[Seq[MatchResult]]
  def add(matcher: Matcher): CompositeMatcher
  def &(matcher: Matcher) = add(matcher)
}

/**
 * @see Matcher
 */
trait AtomicMatcher extends Matcher {
  def name: String
  def add(matcher: Matcher) = new CompositeMatcher()
          .add(this)
          .add(matcher)
}

/**
 * @see Matcher
 */
class CompositeMatcher extends Matcher {
  private var _matchers: Seq[Matcher] = Nil
  def matchers = _matchers
  def add(matcher: Matcher): CompositeMatcher = {
    _matchers ++= List(matcher)
    return this
  }
  def apply() = try {
    val matches = _matchers.flatMap(m => m.apply match {
      case Some(matches: Seq[MatchResult]) => matches
      case _ => throw new MatchError
    })
    if (matches.size > 0) Some(matches)
    else None
  } catch {
    case e: MatchError => None
  }
}


/*!## The `RegexMatcher`

The `RegexMatcher` is designed to provide common request matching functionality to all
matchers.

It can be used either with regular expressions or with String expressions.

When using regular expressions, if match is successful, the matched groups can be
accessed using the `params` method of corresponding `MatchResult`.

When using String expressions, following processing occurs:

  * the characters `.`, `(` and `)` are escaped so that they are not mistreated by
  regex engine;
  * the named parameters like ":param" are recognized within the expression; they
  are transformed into reluctant regex groups `([^/?&#.]+?)` which match any
  characters except `/`, `?`, `?`, `&`, `#` and `.`;
  * all occurences of the `*` character is replaced with reluctant groups `(.*?)`
  which match zero or more characters;
  * all occurences of the `+` character is replaced with reluctant groups `(.+?)`
  which match one or more characters;
  * `?` remains the same and indicates that the preceding character is optional
  for matching (for example, `get("/files/?")` matches both `/files` and `/files/`
  requests).

Then, if match is successful, named parameters are accessible by their name from
the corresponding `MatchResult`. All other parameters are accessible via the `params`
method (note that named parameters are groups too, so they appear inside `params`
and have their index as well).
*/

/**
 * @see Matcher
 */
class RegexMatcher(val name: String,
                   val value: String,
                   protected var regex: Regex,
                   protected var groupNames: Seq[String] = Nil) extends AtomicMatcher {
  def this(name: String, value: String, pattern: String) = {
    this(name, value, null, Nil)
    processPattern(pattern)
  }
  protected def processPattern(pattern: String): Unit = {
    this.groupNames = List("splat")    // for `group(0)`
    this.regex = (""":\w+|[\*+.()]""".r.replaceAllIn(pattern, m => m.group(0) match {
      case "*" | "+" =>
        groupNames ++= List("splat")
        "(." + m.group(0) + "?)"
      case "." | "(" | ")" =>
        "\\\\" + m.group(0)
      case _ =>
        groupNames ++= List(m.group(0).substring(1))
        "([^/?#]+?)"
    })).r
  }
  def groupName(index: Int): String=
    if (groupNames.indices.contains(index)) groupNames(index)
    else "splat"
  def apply(): Option[Seq[MatchResult]] = {
    val m = regex.pattern.matcher(value)
    if (m.matches) {
      val matches = (0 to m.groupCount).map(i => groupName(i) -> m.group(i))
      Some(List(new MatchResult(name, matches: _*)))
    } else None
  }
}

/*! `HeaderMatcher` is used to match the requests by contents of their headers. */

/**
 * @see Matcher
 */
class HeaderMatcher(name: String,
                    regex: Regex,
                    groupNames: Seq[String] = Nil)
        extends RegexMatcher(name, request.headers.getOrElse(name,""), regex, groupNames) {
  def this(name: String, pattern: String) = {
    this(name, null, Nil)
    processPattern(pattern)
  }
}

/*! `HeaderMatcherHelper` provides DSL for matching requests by headers. See `matchers` object
in package `ru.circumflex.web` for more information. */

/**
 * @see Matcher
 */
class HeaderMatcherHelper(name: String) {
  def apply(regex: Regex, groupNames: Seq[String] = Nil) =
    new HeaderMatcher(name, regex, groupNames)
  def apply(pattern: String) = new HeaderMatcher(name, pattern)
}
