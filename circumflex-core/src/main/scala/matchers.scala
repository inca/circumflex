package ru.circumflex.core

import util.matching.Regex

// ## Matching result

class Match(val name: String,
            val params: (String, String)*) extends HashModel {
  def get(index: Int): Option[String] =
    if (params.indices.contains(index)) Some(params(index)._2)
    else None
  def get(name: String): Option[String] = params.find(_._1 == name) match {
    case Some(param: Pair[String, String]) => Some(param._2)
    case _ => None
  }
  def apply(): String = apply(0)
  def apply(index: Int): String = get(index).getOrElse("")
  override def apply(name: String): String = get(name).getOrElse("")
  def splat: Seq[String] = params.filter(_._1 == "splat").map(_._2).toSeq
  override def toString = name
}

// ## Matchers

trait Matcher {
  def apply(): Option[Seq[Match]]
  def add(matcher: Matcher): CompositeMatcher
  def &(matcher: Matcher) = add(matcher)
}

trait AtomicMatcher extends Matcher {
  def name: String
  def add(matcher: Matcher) = new CompositeMatcher()
      .add(this)
      .add(matcher)
}

class CompositeMatcher extends Matcher {
  private var _matchers: Seq[Matcher] = Nil
  def matchers = _matchers
  def add(matcher: Matcher): CompositeMatcher = {
    _matchers ++= List(matcher)
    return this
  }
  def apply() = try {
    val matches = _matchers.flatMap(m => m.apply match {
      case Some(matches: Seq[Match]) => matches
      case _ => throw new MatchError
    })
    if (matches.size > 0) Some(matches)
    else None
  } catch {
    case e: MatchError => None
  }
}

/* ## Basics matcher */

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
    this.regex = (""":\w+|[\*.+()]""".r.replaceAllIn(pattern, m => m.group(0) match {
      case "*" | "+" =>
        groupNames ++= List("splat")
        "(." + m.group(0) + "?)"
      case "." | "(" | ")" =>
        "\\\\" + m.group(0)
      case _ =>
        groupNames ++= List(m.group(0).substring(1))
        "([^/?&#.]+)"
    })).r
  }
  def groupName(index: Int): String=
    if (groupNames.indices.contains(index)) groupNames(index)
    else "splat"
  def apply(): Option[Seq[Match]] = {
    val m = regex.pattern.matcher(value)
    if (m.matches) {
      val matches = for (i <- 0 to m.groupCount) yield groupName(i) -> m.group(i)
      Some(List(new Match(name, matches: _*)))
    } else None
  }
}

class HeaderMatcher(name: String,
                    regex: Regex,
                    groupNames: Seq[String] = Nil)
    extends RegexMatcher(name, ctx.header.getOrElse(name,""), regex, groupNames) {
  def this(name: String, pattern: String) = {
    this(name, null, Nil)
    processPattern(pattern)
  }
}

class HeaderMatcherHelper(name: String) {
  def apply(regex: Regex, groupNames: Seq[String] = Nil) = 
    new HeaderMatcher(name, regex, groupNames)
  def apply(pattern: String) =
    new HeaderMatcher(name, pattern)
}
