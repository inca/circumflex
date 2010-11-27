package ru.circumflex.md

import java.lang.StringBuilder
import java.util.regex.{Pattern, Matcher}
import Markdown._

/**
 * Represents a simple wrapper over `StringBuilder` with utility methods.
 */
class StringEx(protected var text: StringBuilder) {

  def this(source: CharSequence) = this(new StringBuilder(source))

  /**
   * Creates a `Matcher` using specified `Pattern` and applies replacements literally
   * (without interpreting $1, $2, etc.) by calling specified `replacementFunction`
   * on each match.
   */
  def replaceAll(pattern: Pattern, replacementFunction: Matcher => CharSequence): this.type = {
    var lastIndex = 0;
    val m = pattern.matcher(text);
    val sb = new StringBuilder();
    while (m.find()) {
      sb.append(text.subSequence(lastIndex, m.start))
      sb.append(replacementFunction(m))
      lastIndex = m.end
    }
    sb.append(text.subSequence(lastIndex, text.length))
    text = sb;
    return this
  }

  /**
   * Replaces all occurences of specified `string` with specified `replacement`
   * without using regular expressions.
   */
  def replaceAll(string: String, replacement: CharSequence): this.type = {
    val result = new StringBuilder
    var startIdx = 0
    var oldIdx = 0
    oldIdx = text.indexOf(string, startIdx)
    while (oldIdx >= 0) {
      result.append(text.substring(startIdx, oldIdx))
      result.append(replacement)
      startIdx = oldIdx + string.length
      oldIdx = text.indexOf(string, startIdx)
    }
    result.append(text.substring(startIdx))
    text = result
    return this
  }

  def replaceAll(pattern: Pattern, replacement: CharSequence, literally: Boolean = true): this.type =
    if (literally) replaceAll(pattern, m => replacement)
    else {
      text = new StringBuilder(pattern.matcher(text).replaceAll(replacement.toString))
      return this
    }

  /**
   * Appends the specified character sequence.
   */
  def append(s: CharSequence): this.type = {
    text.append(s)
    return this
  }

  /**
   * Prepends the specified character sequence.
   */
  def prepend(s: CharSequence): this.type = {
    text = new StringBuilder(s).append(text)
    return this
  }

  /**
   * Removes at most 4 leading spaces at the beginning of every line.
   */
  def outdent(): this.type = replaceAll(rOutdent, "")

  /**
   * Provides the length of the underlying buffer.
   */
  def length = text.length

  /**
   * Extracts the sub-sequence from underlying buffer.
   */
  def subSequence(start: Int, end: Int) =
    text.subSequence(start, end)

  /**
   * Creates a `Matcher` from specified `pattern`.
   */
  def matcher(pattern: Pattern) = pattern.matcher(text)

  /**
   * Emits the content of underlying buffer.
   */
  override def toString = text.toString
}
