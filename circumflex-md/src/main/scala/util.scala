package ru.circumflex.md

import java.lang.StringBuilder
import java.util.regex.{Pattern, Matcher}
import Markdown._

/* # Character protector */

/**
 * We use character protector mechanism to ensure that certain elements of markup,
 * such as inline HTML blocks, remain undamaged when processing.
 */
class Protector {
  protected var protectHash: Map[String, CharSequence] = Map()
  protected var unprotectHash: Map[CharSequence, String] = Map()

  /**
   * Generates a random hash key.
   */
  def randomKey = (0 to keySize).foldLeft("")((s, i) =>
    s + chars.charAt(rnd.nextInt(keySize)))

  /**
   * Adds the specified token to hash and returns the protection key.
   */
  def addToken(t: CharSequence): String = unprotectHash.get(t) match {
    case Some(key) => key
    case _ =>
      val key = randomKey
      protectHash += key -> t
      unprotectHash += t -> key
      key
  }

  /**
   * Attempts to retrieve an encoded sequence by specified `key`.
   */
  def decode(key: String): Option[CharSequence] = protectHash.get(key)

  override def toString = protectHash.toString
}

/* # Enhanced String Builder */

/**
 * A simple wrapper over `StringBuilder` with utility methods.
 */
class StringEx(protected var text: StringBuilder) {

  def this(source: CharSequence) = this(new StringBuilder(source))

  /**
   * Creates a `Matcher` using specified `Pattern` and applies replacements literally
   * (without interpreting $1, $2, etc.) by calling specified `replacementFunction`
   * on each match.
   */
  def replaceAll(pattern: Pattern, replacementFunction: Matcher => String): this.type = {
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

  def replaceAll(pattern: Pattern, replacement: String): this.type =
    replaceAll(pattern, m => replacement)

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
   * Adds the subsequence between `startIdx` and `endIdx` to specified `protector` and
   * applies a replacement to the `text`.
   */
  def protectSubseq(protector: Protector, startIdx: Int, endIdx: Int): String = {
    val subseq = text.subSequence(startIdx, endIdx)
    val key = protector.addToken(subseq)
    text = new StringBuilder(text.subSequence(0, startIdx))
        .append("\n")
        .append(key)
        .append("\n")
        .append(text.subSequence(endIdx, text.length))
    return key
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