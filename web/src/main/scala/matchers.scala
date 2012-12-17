package pro.savant.circumflex
package web

import util.matching.Regex

/*!# Matchers

The `MatchResult` and `Matcher` are the cornerstone of request routing.

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
class MatchResult(val name: String,
                  val params: (String, String)*) {

  def get(index: Int): Option[String] =
    if (params.indices.contains(index)) Some(params(index)._2)
    else None

  def get(name: String): Option[String] = params.find(_._1 == name).map(_._2)

  def apply(): String = apply(0)

  def apply(index: Int): String = get(index).getOrElse("")

  def apply(name: String): String = get(name).getOrElse("")

  def splat: Seq[String] = params.filter(_._1 == "splat").map(_._2).toSeq

  override def toString = apply(0)
}

/*!## Matcher

The `Matcher` is designed to provide common request matching functionality.

It can be used either with regular expressions or with String expressions.

When using regular expressions, if match is successful, the matched groups can be
accessed using the `params` method of corresponding `MatchResult`.

When using String expressions, following processing occurs:

  * the characters `.`, `(` and `)` are escaped so that they are not mistreated by
  regex engine;

  * the named parameters like ":param" are recognized within the expression; they
  are transformed into greedy regex groups `([^/?&#.]+)` which match any
  characters except `/`, `?`, `?`, `&`, `#` and `.`;

  * all occurrences of the `*` character are replaced with reluctant groups `(.*?)`
  which match zero or more characters;

  * all occurrences of the `+` character are replaced with reluctant groups `(.+?)`
  which match one or more characters;

  * `?` remains the same and indicates that the preceding character is optional
  for matching (for example, `get("/files/?")` matches both `/files` and `/files/`
  requests).

Then, if match is successful, named parameters are accessible by their name from
the corresponding `MatchResult`. All other parameters are accessible via the `params`
method (note that named parameters are groups too, so they appear inside `params`
and have their index as well).
*/
class Matcher(val name: String,
              val value: String,
              protected var regex: Regex,
              protected var groupNames: Seq[String] = Nil) {

  def this(name: String, value: String, pattern: String) = {
    this(name, value, null, Nil)
    processPattern(pattern)
  }

  protected def processPattern(pattern: String) {
    this.groupNames = List("splat")    // for `group(0)`
    this.regex = (""":\w+|[\*+.()]""".r.replaceAllIn(pattern, m => m.group(0) match {
      case "*" | "+" =>
        groupNames ++= List("splat")
        "(." + m.group(0) + "?)"
      case "." | "(" | ")" =>
        "\\\\" + m.group(0)
      case _ =>
        groupNames ++= List(m.group(0).substring(1))
        "([^/?#]+)"
    })).r
  }

  def groupName(index: Int): String=
    if (groupNames.indices.contains(index)) groupNames(index)
    else "splat"

  def apply: Option[Seq[MatchResult]] = {
    val m = regex.pattern.matcher(value)
    if (m.matches) {
      val matches = (0 to m.groupCount).map(i => groupName(i) -> m.group(i))
      Some(List(new MatchResult(name, matches: _*)))
    } else None
  }

  def matchPrefix: Option[(String, Seq[MatchResult])] = {
    val m = regex.pattern.matcher(value)
    if (m.lookingAt) {
      val matches = (0 to m.groupCount).map(i => groupName(i) -> m.group(i))
      Some(value.substring(m.start, m.end) -> Seq(new MatchResult(name, matches: _*)))
    } else None
  }
}

class UriMatcher(regex: Regex, groupNames: Seq[String] = Nil)
    extends Matcher("uri", request.uri, regex, groupNames) {

  def this(pattern: String) = {
    this(null, Nil)
    processPattern(pattern)
  }

}

/*! `HeaderMatcher` is used to match the requests by contents of their headers. */
class HeaderMatcher(name: String, regex: Regex, groupNames: Seq[String] = Nil)
    extends Matcher(name, request.headers.getOrElse(name,""), regex, Nil) {

  def this(name: String, pattern: String) = {
    this(name, null, Nil)
    processPattern(pattern)
  }

}

/*! `HeaderMatcherHelper` provides DSL for matching requests by headers. See `matchers` object
in package `pro.savant.circumflex.web` for more information. */
class HeaderMatcherHelper(name: String) {

  def apply(regex: Regex) = new HeaderMatcher(name, regex, Nil)

  def apply(pattern: String) = new HeaderMatcher(name, pattern)

}
