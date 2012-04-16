package ru.circumflex
package markeven

import collection.mutable.ListBuffer
import java.util.regex.{Pattern, Matcher}

// Generic Character Walker

trait Walker extends CharSequence {

  def rangeStr = "[" + 0 + "," + length + ")"

  def checkIndex(index: Int) {
    if (index < 0 || index >= length)
      throw new IndexOutOfBoundsException(
        getClass.getSimpleName + " " + rangeStr + " does not contain index " + index)
  }

  def getAbsolutePosition: Int = getAbsoluteIndex(position)

  def getAbsoluteIndex(relative: Int): Int = {
    checkIndex(relative)
    relative
  }

  def checkRange(startIdx: Int, endIdx: Int) {
    if (startIdx > endIdx)
      throw new IllegalStateException("Start index cannot be greater than end index.")
    if (startIdx < 0 || startIdx >= length)
      throw new IndexOutOfBoundsException(
        getClass.getSimpleName + " " + rangeStr + " does not contain index " + startIdx)
    if (endIdx < 0 || endIdx > length)
      throw new IndexOutOfBoundsException(
        getClass.getSimpleName + " " + rangeStr + " does not contain index " + endIdx)
  }

  // Basic iteration

  private var _pos = 0
  def position = _pos
  def current = charAt(_pos)
  def hasCurrent = _pos >= 0 && _pos < length
  def hasNext = (_pos + 1) < length
  def skip(n: Int): this.type = {
    _pos += n
    this
  }
  def skip(): this.type = skip(1)
  def peek: Char = charAt(_pos + 1)
  def reset(): this.type = {
    _pos = 0
    this
  }
  def startFrom(i: Int): this.type = {
    _pos = i
    this
  }
  def stepBack(): this.type = {
    _pos -= 1
    this
  }

  // Lookahead

  // Execute `func`, return its result and revert the position
  // to the start state.
  def lookahead[A](func: Walker => A): A = {
    val oldPos = _pos
    val result = func(this)
    _pos = oldPos
    result
  }

  // Subregioning and exclusion

  def exclude(startIdx: Int, endIdx: Int): Walker = {
    checkRange(startIdx, endIdx)
    if (endIdx - startIdx == 0)
      return this
    if (startIdx == 0 && endIdx == this.length)
      return new EmptyWalker()
    if (startIdx == 0)
      return new SubSeqWalker(this, endIdx)
    if (endIdx == this.length)
      return new SubSeqWalker(this, 0, startIdx)
    new MultiSeqWalker(Seq(
      new SubSeqWalker(this, 0, startIdx), new SubSeqWalker(this, endIdx)))
  }

  // Basic matching

  // Am I looking at specified `cs`?
  def at(cs: CharSequence): Boolean = {
    var success = true
    var i = 0
    while(success && i < cs.length) {
      if (!hasCurrent) success = false
      else if (current != cs.charAt(i)) success = false
      else {
        i += 1
        skip()
      }
    }
    // recover iterator position
    _pos = _pos - i
    success
  }

  // Am I looking at digit [0-9]?
  def atDigit: Boolean = {
    hasCurrent && current.isDigit
  }

  // Regex helpers
  def at(pattern: Pattern): Option[Matcher] = {
    val m = pattern.matcher(this)
    m.region(position, length)
    m.useTransparentBounds(true)
    if (m.lookingAt) Some(m)
    else None
  }

  def atMatch(pattern: Pattern): Boolean = {
    val m = pattern.matcher(this)
    m.region(position, length)
    m.matches
  }

  // New lines
  def atNewLine: Boolean = at("\r\n") || at("\n") || at("\r")
  def skipNewLine(): this.type = {
    if (at("\r\n"))
      _pos += 2
    else if (atNewLine)
      _pos += 1
    this
  }
  def skipNewLines(): this.type = {
    while (atNewLine) skipNewLine()
    this
  }

  // Spaces and tabs
  def atSpace: Boolean = at(" ") || at("\t")
  def skipSpace(): this.type = {
    if (atSpace) _pos += 1
    this
  }
  def skipSpaces(): this.type = {
    while (atSpace) _pos += 1
    this
  }

  // Space counter
  def atSpaces(count: Int): Boolean = lookahead { w =>
    var found = true
    var i = count
    while (found && i > 0) {
      if (!w.hasCurrent) found = false
      else {
        if (w.at(" ")) {
          i -= 1
          w.skip()
        } else found = false
      }
    }
    found && i == 0
  }

  // Whitespace in general
  def atWhitespace: Boolean = atNewLine || atSpace
  def skipWhitespace(): this.type = {
    val p = _pos
    skipNewLine()
    if (p == _pos) skipSpace()
    this
  }
  def skipWhitespaces(): this.type = {
    while (atWhitespace) skipWhitespace()
    this
  }

  // Blank lines
  def skipBlankLines(): this.type = {
    var finish = false
    while (!finish) {
      val p = _pos
      skipSpaces()
      // now there must be newline, otherwise the line is not blank
      if (atNewLine) {
        skipNewLines()
      } else {
        _pos = p
        finish = true
      }
    }
    this
  }

}

class EmptyWalker extends Walker {
  def length = 0
  def charAt(index: Int) =
    throw new IndexOutOfBoundsException("EmptyWalker does not contain any characters.")
  def subSequence(startIdx: Int, endIdx: Int) =
    throw new IndexOutOfBoundsException("EmptyWalker does not contain any subsequences.")
}

class SubSeqWalker(val input: CharSequence, s: Int, val e: Int)
    extends Walker {

  def this(input: CharSequence, s: Int) = this(input, s, input.length)
  def this(input: CharSequence) = this(input, 0)

  override def getAbsoluteIndex(relative: Int) = {
    checkIndex(relative)
    start + relative
  }

  // Char sequence

  val start = if (s < 0) 0 else s
  val end = if (e > input.length) input.length else e
  def length = end - start
  def charAt(index: Int) = {
    checkIndex(index)
    input.charAt(start + index)
  }
  def subSequence(startIdx: Int, endIdx: Int) = {
    checkRange(startIdx, endIdx)
    new SubSeqWalker(input, start + startIdx, start + endIdx)
  }
  override def toString = input.subSequence(start, end).toString
}

class MultiSeqWalker(val regions: Seq[CharSequence]) extends Walker {
  val length = regions.map(_.length).sum

  override def getAbsoluteIndex(relative: Int) = {
    checkIndex(relative)
    var i = 0
    var idx = relative
    while (idx >= regions(i).length) {
      idx -= regions(i).length
      i += 1
    }
    regions(i) match {
      case w: Walker => w.getAbsoluteIndex(idx)
      case _ => idx
    }
  }

  def charAt(index: Int) = {
    checkIndex(index)
    var i = 0
    var idx = index
    while (idx >= regions(i).length) {
      idx -= regions(i).length
      i += 1
    }
    regions(i).charAt(idx)
  }

  def subSequence(startIdx: Int, endIdx: Int): CharSequence = {
    checkRange(startIdx, endIdx)
    if (startIdx == endIdx) return ""
    val buffer = new ListBuffer[CharSequence]
    var l = endIdx - startIdx
    var o = startIdx
    var i = 0
    // find start region
    while (o >= regions(i).length) {
      o -= regions(i).length
      i += 1
    }
    // append start and middle regions, if any
    while (i < regions.length && (o + l) >= regions(i).length) {
      val w = if (o == 0) regions(i) else new SubSeqWalker(regions(i), o)
      buffer += w
      l -= w.length
      o = 0
      i += 1
    }
    // append end region, if any
    if (l > 0)
      buffer += new SubSeqWalker(regions(i), o, o + l)
    // return result
    if (buffer.size == 1) buffer(0)
    else new MultiSeqWalker(buffer)
  }

  override def toString = regions.mkString("")
}
