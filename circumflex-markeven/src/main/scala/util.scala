package ru.circumflex.markeven

import java.lang.StringBuilder
import java.util.regex._

/*!# Character protector

We use character protector mechanism to ensure that certain elements of markup,
such as inline HTML blocks, remain undamaged when processing.
*/
class Protector {
  protected var protectHash: Map[String, CharSequence] = Map()
  protected var unprotectHash: Map[CharSequence, String] = Map()


  def randomKey = "!}" + (0 until keySize).foldLeft("")((s, i) =>
    s + chars.charAt(rnd.nextInt(chars.length)))


  def addToken(t: CharSequence): String = unprotectHash.get(t) match {
    case Some(key) => key
    case _ =>
      val key = randomKey
      protectHash += key -> t
      unprotectHash += t -> key
      key
  }


  def decode(key: String): Option[CharSequence] = protectHash.get(key)


  def keys = protectHash.keys


  def clear: Unit = {
    protectHash = Map()
    unprotectHash = Map()
  }

  override def toString = protectHash.toString
}

/*!# Mutable Character Buffer

We use `StringEx` internally for various text processing stuff. It wraps a mutable
`buffer: StringBuilder` and enhances it with `replaceAll` and `replaceIndexed` functionality.
The rationale behind low-level replacements is that `java.util.regex.Matcher` supports
`StringBuffer` in `appendReplacement` and `appendTail` methods. Since our processing environment
does not support multithreading, we avoid synchronization costs using `StringBuilder` instead.
*/
class StringEx(var buffer: StringBuilder) extends CharSequence with Cloneable {
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
    val sb = new StringBuilder(buffer.length)
    while (m.find()) {
      sb.append(buffer.subSequence(lastIndex, m.start))
      sb.append(replacement(m))
      lastIndex = m.end
    }
    sb.append(buffer.subSequence(lastIndex, buffer.length))
    this.buffer = sb
    return this
  }

  def replaceFirst(pattern: Pattern, replacement: Matcher => CharSequence): Int = {
    var length = -1
    val m = pattern.matcher(buffer)
    if (m.find()) {
      val sb = new StringBuilder(buffer.length)
      sb.append(buffer.subSequence(0, m.start))
      sb.append(replacement(m))
      sb.append(buffer.subSequence(m.end, buffer.length))
      this.buffer = sb
      length = m.end - m.start
    }
    return length
  }

  def replaceFirst(pattern: Pattern, replacement: CharSequence): Int =
    replaceFirst(pattern, m => replacement)

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

  def split(pattern: Pattern): Seq[StringEx] =
    pattern.split(buffer).map(s => new StringEx(s))

  def append(cs: CharSequence): this.type = {
    buffer.append(cs)
    return this
  }

  def prepend(cs: CharSequence): this.type = {
    buffer.replace(0, 0, cs.toString)
    return this
  }

  def outdent(): this.type = replaceAll(regexes.outdent(4), "")

  def substring(start: Int, end: Int = length): this.type = {
    buffer = new StringBuilder(subSequence(start, end))
    return this
  }

  def startsWith(cs: CharSequence): Boolean = {
    if (cs.length == 0) return true
    if (cs.length > buffer.length) return false
    var i = 0
    while (i < cs.length) {
      if (cs.charAt(i) != buffer.charAt(i))
        return false
      i += 1
    }
    return true
  }

  def endsWith(cs: CharSequence): Boolean = {
    if (cs.length == 0) return true
    if (cs.length > buffer.length) return false
    var i = 1
    while (i <= cs.length) {
      if (cs.charAt(cs.length - i) != buffer.charAt(buffer.length - i))
        return false
      i += 1
    }
    return true
  }

  def equals(cs: CharSequence): Boolean = {
    if (cs.length != buffer.length)
      return false
    return startsWith(cs)
  }

  def trimLeft(): Int = {
    var i = 0
    while ((i < buffer.length) && (buffer.charAt(i) <= ' '))
      i += 1
    if (i > 0)
      buffer.delete(0, i)
    return i
  }
  
  def trimRight(): Int = {
    var i = 0
    while ((i < buffer.length) && (buffer.charAt(buffer.length - i - 1) <= ' '))
      i += 1
    if (i > 0) buffer.delete(buffer.length - i, buffer.length)
    return i
  }

  def trim(): this.type = {
    trimLeft
    trimRight
    return this
  }

  def matches(pattern: Pattern): Boolean =
    pattern.matcher(buffer).matches

  def matches[B](pattern: Pattern, function: Matcher => B): Option[B] = {
    val m = pattern.matcher(buffer)
    if (m.matches) Some(function(m))
    else None
  }

  override def clone: StringEx = super.clone.asInstanceOf[StringEx]

  // inherited from `CharSequence`

  def length: Int = buffer.length

  def subSequence(start: Int, end: Int): CharSequence = buffer.subSequence(start, end)

  def charAt(index: Int): Char = buffer.charAt(index)

  override def toString = buffer.toString

}

class ChunkIterator(val chunks: Seq[StringEx]) {
  private var _index = -1
  def index = _index
  def hasNext: Boolean = (_index + 1) < chunks.length
  def next: StringEx = {
    _index += 1
    return chunks(_index)
  }
  def peek: StringEx = chunks(_index + 1)
  def reset: this.type = {
    _index = -1
    return this
  }
  def stepBack: Unit = if (_index > -1) _index -= 1
  def size = chunks.size
}

class CharIterator(val chunk: StringEx) {
  private var _index = -1
  def index = _index
  def hasNext: Boolean = (_index + 1) < chunk.length
  def next: Char = {
    _index += 1
    return chunk.charAt(_index)
  }
  def peek: Char = chunk.charAt(_index + 1)
  def reset: this.type = {
    _index = -1
    return this
  }
  def startFrom(i: Int): this.type = {
    _index = i - 1
    return this
  }
  def stepBack: Unit = if (_index > -1) _index -= 1
  def size = chunk.length
}