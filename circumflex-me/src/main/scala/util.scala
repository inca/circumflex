package ru.circumflex.me

import java.lang.StringBuilder
import java.util.regex._

/*!# Character protector

We use character protector mechanism to ensure that certain elements of markup,
such as inline HTML blocks, remain undamaged when processing.
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

  /**
   * Returns hash keys that are currently in use.
   */
  def keys = protectHash.keys

  override def toString = protectHash.toString
}

/*!# Mutable Character Buffer

We use `StringEx` internally for various text processing stuff. It wraps a mutable `buffer: StringBuilder`
and enhances it with `replaceAll` and `replaceIndexed` functionality. The rationale behind low-level replacements
is that `java.util.regex.Matcher` supports `StringBuffer` in `appendReplacement` and `appendTail` methods.
Since our processing environment does not support multithreading, we avoid synchronization costs using `StringBuilder`
instead.
*/
class StringEx(var buffer: StringBuilder) {
  def this(cs: CharSequence) = this(new StringBuilder(cs))

  // A tricky one: uses specified `pattern` to point the start of replacement and lets
  // `replacement` function determine the end index of replacement.
  def replaceIndexed(pattern: Pattern, replacement: Matcher => (CharSequence, Int)): this.type = {
    var startIndex = 0
    val m = pattern.matcher(buffer)
    while (m.find(startIndex)) {
      val r = replacement(m)
      val text = r._1.toString
      val endIdx = r._2
      // apply replacement
      buffer.replace(m.start, endIdx, text)
      // evaluate new start index
      startIndex = m.start + text.length
    }
    return this
  }

  def replaceAll(pattern: Pattern, replacement: Matcher => CharSequence): this.type = {
    var lastIndex = 0;
    val m = pattern.matcher(buffer)
    val sb = new StringBuilder()
    while (m.find()) {
      sb.append(buffer.subSequence(lastIndex, m.start))
      sb.append(replacement(m))
      lastIndex = m.end
    }
    sb.append(buffer.subSequence(lastIndex, buffer.length))
    this.buffer = sb
    return this
  }

  def replaceAll(pattern: Pattern, replacement: CharSequence): this.type =
    replaceAll(pattern, m => replacement)

  def replaceAll(text: String, replacement: CharSequence): this.type = {
    if (text.length == 0) return this
    var i = buffer.indexOf(text)
    while (i != -1) {
      buffer.replace(i, i + text.length, replacement.toString)
      i = buffer.indexOf(text, i + replacement.length)
    }
    return this
  }

  def outdent(): this.type = replaceAll(regexes.outdent, m => "")

  def append(cs: CharSequence): this.type = {
    buffer.append(cs)
    return this
  }

  def prepend(cs: CharSequence): this.type = {
    buffer.replace(0, 0, cs.toString)
    return this
  }

  def length: Int = buffer.length

  override def toString = buffer.toString 
}