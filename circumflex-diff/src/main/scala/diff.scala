package ru.circumflex.diff

import java.lang.StringBuilder
import collection.mutable.{HashMap, ListBuffer}

/*!# Diff Utils

Circumflex Diff Util is a Scala port of a part of [google-diff-patch-match][dpm] project by
[Neil Fraser](http://neil.fraser.name).

  [dpm]: http://code.google.com/p/google-diff-match-patch/

`DiffProcessor` is used to find differences between two strings. Difference is expressed as a
sequence of `Diff` objects which contain an operation to be applied to a portion of `text1` to
produce `text2`. Operations are either one of `INSERT`, `DELETE` and `EQUAL`.

`DiffProcessor` is configured with following variables:

  * `timeout` is an amount of time (in seconds) to map a diff before giving up;
  * `editCost` measures an empty edit operation in terms of edit characters;
  * `dualThreshold` is a size beyound which the fast but less accurate double-ended algorithm activates.
*/

case class Operation(description: String) {
  override def toString = description
}

object Operation {
  object INSERT extends Operation("Insert")
  object DELETE extends Operation("Delete")
  object EQUAL extends Operation("Equal")
}

case class Diff(operation: Operation, text: String) {
  override def toString = operation + ": " + text.replace('\n', '\u00b6')
}

class DiffProcessor(val timeout: Float = 0f,
                    val editCost: Int = 4,
                    val dualThreshold: Int = 32) {

  protected def calcDeadline: Long =
    if (timeout <= 0) java.lang.Long.MAX_VALUE else
      System.currentTimeMillis + (timeout * 1000).toLong

  def findDifference(text1: String,
                     text2: String,
                     checkLines: Boolean,
                     deadline: Long = calcDeadline): Seq[Diff] = {
    // speedups: check for equality and trim common suffixes and prefixes
    if (text1 == text2) return List(Diff(Operation.EQUAL, text1))
    val prefix = text1.substring(0, commonPrefix(text1, text2))
    val suffix = text1.substring(text1.length - commonSuffix(text1, text2))
    // compute diffs on middle block
    var diffs = compute(text1.substring(prefix.length, text1.length - suffix.length),
      text2.substring(prefix.length, text2.length - suffix.length),
      checkLines, deadline)
    if (prefix.length > 0)
      diffs = List(Diff(Operation.EQUAL, prefix)) ++ diffs
    if (suffix.length > 0)
      diffs = diffs ++ List(Diff(Operation.EQUAL, suffix))
    return merge(diffs)
  }

  def commonPrefix(text1: String, text2: String): Int = {
    val n = math.min(text1.length, text2.length)
    for (i <- 0 until n)
      if (text1.charAt(i) != text2.charAt(i))
        return i
    return n
  }

  def commonSuffix(text1: String, text2: String): Int = {
    val n = math.min(text1.length, text2.length)
    for (i <- 1 to n)
      if (text1.charAt(text1.length - i) != text2.charAt(text2.length - i))
        return i - 1
    return n
  }

  // determine whether a suffix of `text1` is a prefix of `text2`
  protected def commonOverlap(text1: String, text2: String): Int = {
    if (text1.length == 0 || text2.length == 0) return 0
    // truncate longer strings
    if (text1.length > text2.length)
      return commonOverlap(text1.substring(text1.length - text2.length), text2)
    if (text1.length < text2.length)
      return commonOverlap(text1, text2.substring(0, text1.length))
    // check the equality
    if (text1 == text2) return text1.length
    // look for patterns
    var best = 0
    var length = 1
    while (true) {
      val pattern = text1.substring(text1.length - length)
      val i = text2.indexOf(pattern)
      if (i == -1) return best
      length += i
      if (i == 0 || text1.substring(text1.length - length).equals(text2.substring(0, length))) {
        best = length
        length += 1
      }
    }
    return best
  }

  // main diff computation
  protected def compute(text1: String, text2: String, checkLines: Boolean, deadline: Long): Seq[Diff] = {
    // speedups: empty inputs
    if (text1.length == 0)
      return List(Diff(Operation.INSERT, text2))
    if (text2.length == 0)
      return List(Diff(Operation.DELETE, text1))
    // speedup: text inside text
    val shortest = if (text1.length > text2.length) text2 else text1
    val longest = if (text1.length > text2.length) text1 else text2
    val i = longest.indexOf(shortest)
    if (i != -1) {
      val op = if (text1.length > text2.length) Operation.DELETE else Operation.INSERT
      return List(Diff(op, longest.substring(0, i)),
        Diff(Operation.EQUAL, shortest),
        Diff(op, longest.substring(i + shortest.length)))
    }
    // maybe we can split the task in two?
    halfMatch(text1, text2) map { hm =>
      return findDifference(hm.prefix1, hm.prefix2, checkLines) ++
          List(Diff(Operation.EQUAL, hm.common)) ++
          findDifference(hm.suffix1, hm.suffix2, checkLines)
    }
    // determine if checkLines algorithm is efficient enough
    val chkLines = checkLines && (text1.length > 100) && (text2.length > 100)
    if (!chkLines) return mapDiff(text1, text2, deadline)
    else {
      val encodedLines = encodeLines(text1, text2)
      var diffs = mapDiff(encodedLines.chars1, encodedLines.chars2, deadline)
      diffs = decodeLineDiffs(diffs, encodedLines.lines)
      diffs = cleanupSemantic(diffs)
      // rediff replacement blocks, now char-by-char
      var countDelete = 0
      var countInsert = 0
      var textDelete = ""
      var textInsert = ""
      val pointer = new DiffIterator(diffs)
      while (pointer.hasNext) {
        val diff = pointer.next
        diff.operation match {
          case Operation.INSERT =>
            countInsert += 1
            textInsert += diff.text
          case Operation.DELETE =>
            countDelete += 1
            textDelete += diff.text
          case Operation.EQUAL =>
            if (countDelete > 0 && countInsert > 0) {
              // upon reaching equality check for prior redundancies
              (0 until (countDelete + countInsert)).foreach { i =>
                pointer.previous
                pointer.remove
              }
              findDifference(textDelete, textInsert, false, deadline).foreach(d => pointer.add(d))
            }
            countDelete = 0
            countInsert = 0
            textDelete = ""
            textInsert = ""
        }
      }
      return Nil
    }
  }

  // do the two strings share a substring which is at least half the length of longest text?
  def halfMatch(text1: String, text2: String): Option[HalfMatch] = {
    val shortest = if (text1.length > text2.length) text2 else text1
    val longest = if (text1.length > text2.length) text1 else text2
    if (longest.length < 4 || shortest.length * 2 < longest.length) return None
    // look for halfmatch in second quarter of longest text
    val hm1 = halfMatchInternal(longest, shortest, (longest.length + 3) / 4)
    // look for halfmatch in third quarter of longest text
    val hm2 = halfMatchInternal(longest, shortest, (longest.length + 1) / 2)
    // determine, which halfmatch is best if multiple found
    val best = (hm1, hm2) match {
      case (Some(hm1), Some(hm2)) =>
        if (hm1.common.length > hm2.common.length) Some(hm1) else Some(hm2)
      case _ => hm1.orElse(hm2)
    }
    // sort out halfmatch pieces
    best.map { hm =>
      if (text1.length > text2.length) hm
      else hm.inverse
    }
  }

  protected def halfMatchInternal(longest: String, shortest: String, i: Int): Option[HalfMatch] = {
    val seed = longest.substring(i, i + longest.length / 4)
    var best = HalfMatch("", "", "", "", "")
    var j = shortest.indexOf(seed)
    while (j != -1) {
      val prefixLength = commonPrefix(longest.substring(i), shortest.substring(j))
      val suffixLength = commonSuffix(longest.substring(0, i), shortest.substring(0, j))
      if (best.common.length < suffixLength + prefixLength) {
        best = HalfMatch(longest.substring(0, i - suffixLength), // prefix of longest
          longest.substring(i + prefixLength),    // suffix of longest
          shortest.substring(0, j - suffixLength), // prefix of shortest
          shortest.substring(j + prefixLength),    // suffix of shortest
          shortest.substring(j - suffixLength, j + prefixLength))  // common middle
      }
      j = shortest.indexOf(seed, j + 1)
    }
    if (best.common.length >= longest.length / 2) return Some(best)
    else return None
  }

  protected def encodeLines(text1: String, text2: String): EncodedLines = {
    val lines = ListBuffer[String]("")
    val hashes = new HashMap[String, Int]
    return EncodedLines(
      linesToChars(text1, lines, hashes),
      linesToChars(text2, lines, hashes),
      lines)
  }

  protected def decodeLineDiffs(diffs: Seq[Diff], lines: Seq[String]): Seq[Diff] = diffs.map { diff =>
    val sb = new StringBuilder
    for (i <- 0 until diff.text.length)
      sb.append(lines(diff.text(i).toInt))
    Diff(diff.operation, sb.toString)
  }

  protected def linesToChars(text: String,
                             lines: ListBuffer[String],
                             hashes: HashMap[String, Int]): String = {
    var lineStart = 0
    var lineEnd = -1
    var line = ""
    val chars = new StringBuilder
    while (lineEnd < text.length - 1) {
      lineEnd = text.indexOf('\n', lineStart)
      if (lineEnd == -1)
        lineEnd = text.length - 1
      line = text.substring(lineStart, lineEnd + 1)
      lineStart = lineEnd + 1
      hashes.get(line) match {
        case Some(h) => chars.append(h.toChar)
        case _ =>
          lines += line
          val h = lines.size - 1
          hashes += (line -> h)
          chars.append(h.toChar)
      }
    }
    return chars.toString
  }

  // find intersection points between two texts (not revisited)
  protected def mapDiff(text1: String, text2: String, deadline: Long): Seq[Diff] = {
    val max_d = text1.length + text2.length - 1
    val double_end = dualThreshold * 2 < max_d
    var v_map_d: HashMap[Int, Int] = null
    var v_map1: ListBuffer[HashMap[Int, Int]] = ListBuffer()
    var v_map2: ListBuffer[HashMap[Int, Int]] = ListBuffer()
    var v1: HashMap[Int, Int] = HashMap()
    var v2: HashMap[Int, Int] = HashMap()
    v1 += (1 -> 0)
    v2 += (1 -> 0)
    var x = 0
    var y = 0
    var footstep = ""
    val footsteps: HashMap[String, Int] = HashMap() // TODO switch to [Long, Int]
    var done = false
    // if total number of chars is odd, then the front path will collide
    // with reverse path
    val front = (text1.length + text2.length) % 2 == 1
    for (d <- 0 until max_d) {

      def checkFootstep1 = if (double_end) {
        footstep = footprint(x, y)
        if (front && footsteps.contains(footstep)) done = true
        if (!front) footsteps += (footstep -> d)
      }
      def checkFootstep2 = if (double_end) {
        footstep = footprint(text1.length - x, text2.length - y)
        if (!front && footsteps.contains(footstep)) done = true
        if (front) footsteps += (footstep -> d)
      }

      if (System.currentTimeMillis > deadline)    // hit timeout
        return bail(text1, text2)
      // walk the front path, one step
      v_map_d = HashMap()
      v_map1 += v_map_d   // adds at index `d`
      for (k <- Range.inclusive(-d, d, 2)) {
        x = if (k == -d || k != d && v1(k - 1) < v1(k + 1))
          v1(k + 1) else v1(k - 1) + 1
        y = x - k
        checkFootstep1
        while (!done && x < text1.length
            && y < text2.length
            && text1.charAt(x) == text2.charAt(y)) {
          x += 1
          y += 1
          checkFootstep1
        }
        v1 += k -> x
        v_map_d += k -> x
        if (x == text1.length && y == text2.length) // reached end in single-path mode
          return mapDiffPath1(v_map1, text1, text2)
        else if (done) {    // front path ran over reverse path
          v_map2 = v_map2.take(footsteps(footstep) + 1)
          return mapDiffPath1(v_map1, text1.substring(0, x), text2.substring(0, y)) ++
              mapDiffPath2(v_map2, text2.substring(x), text2.substring(y))
        }
      }
      if (double_end) {   // walk the reverse path, one step
        v_map_d = HashMap()
        v_map2 += v_map_d
        for (k <- Range.inclusive(-d, d, 2)) {
          x = if (k == -d || k != d && v2(k - 1) < v2(k + 1))
            v2(k + 1) else v2(k - 1) + 1
          y = x - k
          checkFootstep2
          while (!done && x < text1.length
              && y < text2.length
              && text1.charAt(text1.length - x - 1) == text2.charAt(text2.length - y - 1)) {
            x += 1
            y += 1
            checkFootstep2
          }
          v2 += k -> x
          v_map_d -> k -> x
          if (done) {   // reverse path ran over front path
            v_map1 = v_map1.take(footsteps(footstep) + 1)
            return mapDiffPath1(v_map1, text1.substring(0, text1.length - x), text1.substring(0, text2.length - y)) ++
                mapDiffPath2(v_map2, text1.substring(text1.length - x), text2.substring(text2.length - y))
          }
        }
      }
    }
    // no commonalities found
    return bail(text1, text2)
  }

  protected def mapDiffPath1(v_map: ListBuffer[HashMap[Int, Int]],
                             text1: String,
                             text2: String): Seq[Diff] = {
    Nil
  }

  protected def mapDiffPath2(v_map: ListBuffer[HashMap[Int, Int]],
                             text1: String,
                             text2: String): Seq[Diff] = {
    Nil
  }

  protected def bail(text1: String, text2: String): Seq[Diff] =
    List(Diff(Operation.DELETE, text1), Diff(Operation.INSERT, text2))

  protected def footprint(x: Int, y: Int): String =
    ((x.toLong << 32) + y).toString

  def cleanupSemantic(diffs: Seq[Diff]): Seq[Diff] = {
    // TODO
    return Nil
  }

  def merge(diffs: Seq[Diff]): Seq[Diff] = {
    // TODO
    return diffs
  }

}

