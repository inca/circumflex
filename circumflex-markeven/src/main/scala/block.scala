package pro.savant.circumflex
package markeven

import java.io.Writer
import collection.mutable.ListBuffer

/*!# Block processor

*/
class BlockProcessor(val out: Writer, val conf: MarkevenConf = EmptyMarkevenConf)
    extends Processor {

  val inline = new InlineProcessor(out, conf)

  var selector = new Selector(conf)
  var blockIndent = 0

  def run(walk: Walker) {
    while (walk.hasCurrent)
      block(walk)
  }

  /*! Each time `block` is invoked it should spit out the whole rendered block
   to `out` and move the `walk` to the end of that block.

  Each tryXXX method should:

    * return `false` and restore `in.position` to original value if
      block does not match;
    * return `true` if block matches, this case implies writing block
      markup to `out` and wrapping `in.position` to the end of the block;
  */
  def block(walk: Walker) {
    walk.skipBlankLines()
    countBlockIndent(walk)
    if (tryFragmentBlock(walk)) return
    if (tryCodeBlock(walk)) return
    if (tryDiv(walk)) return
    if (tryHtml(walk)) return
    if (tryUnorderedList(walk)) return
    if (tryOrderedList(walk)) return
    if (tryHrTable(walk)) return
    if (tryHeading(walk)) return
    paragraph(walk)
  }

  /*! Indent is count by looking at the leading whitespace of
  the first non-blank line of the block. Tab indents are not allowed. */
  def countBlockIndent(walk: Walker) {
    blockIndent = 0
    var stop = false
    while (!stop && walk.hasCurrent) {
      if (walk.at(" ")) {
        blockIndent += 1
        walk.skip()
      } else stop = true
    }
  }

  /*! Scrolls walker to the start of the empty line, which usually terminates
  the block. */
  def scrollToTerm(walk: Walker) {
    var found = false
    while (!found && walk.hasCurrent)
      if (walk.atNewLine && walk.skipNewLine().skipSpaces().atNewLine) {
        walk.skipNewLine()
        found = true
      } else walk.skip()
  }

  /*! Selector expression is stripped from each block, if it exists. This may
  result in new walker instantiation.*/
  def stripSelector(walk: Walker): Walker = {
    selector = new Selector(conf)
    val startIdx = walk.position
    while (walk.hasCurrent && !walk.atNewLine) {
      if (walk.at("{")) {
        walk.at(const.selector) match {
          case Some(m) =>
            val id = if (m.group(1) != null) m.group(1).substring(1) else ""
            val classes = new ListBuffer[String]
            if (m.group(2) != null) {
              classes ++= m.group(2).split("\\.").filter(_ != "")
            }
            selector = new Selector(conf, id, classes)
            walk.startFrom(startIdx)
            return walk.exclude(m.start, m.end)
          case _ =>
            walk.skip()
        }
      } else walk.skip()
    }
    // no selector found
    walk.startFrom(startIdx)
    walk
  }

  /*! Unordered list should start with `* `. Every line indented beyond the marker
  is included into list. */
  def tryUnorderedList(walk: Walker): Boolean = {
    if (walk.at("* ")) {
      val startIdx = walk.position
      var found = false
      // scroll to the end of the block
      while (!found && walk.hasCurrent) {
        scrollToTerm(walk)
        walk.skipBlankLines()
        if (walk.atSpaces(blockIndent)) {
          val i = walk.position
          walk.skip(blockIndent)
          if (!walk.at("* ") && !walk.atSpace) {
            found = true
            walk.startFrom(i)
          }
        } else found = true
      }
      // we got UL region, process it
      val ul = stripSelector(new SubSeqWalker(walk, startIdx, walk.position))
      processUl(ul)
      true
    } else false
  }

  def processUl(walk: Walker) {
    assert(walk.at("* "))
    out.write("<ul")
    selector.writeAttrs(out, walk.getAbsolutePosition)
    out.write(">")
    // determining the bounds of each li
    // skip the marker, must point to the start of first li
    walk.skip(2)
    var startIdx = walk.position
    while (walk.hasCurrent) {
      if (walk.atNewLine &&
          walk.skipBlankLines().atSpaces(blockIndent) &&
          walk.skip(blockIndent).at("* ")) {
        val li = stripSelector(new SubSeqWalker(walk, startIdx, walk.position))
        processLi(li)
        walk.skip(2)
        startIdx = walk.position
      } else walk.skip()
    }
    // flush last li
    val li = stripSelector(new SubSeqWalker(walk, startIdx, walk.position))
    processLi(li)
    // all li are done
    out.write("</ul>")
  }

  /*! Ordered list should start with `1. `. Every line indented beyond the marker
  is included into list. */
  def tryOrderedList(walk: Walker): Boolean = {
    if (walk.at("1. ")) {
      val startIdx = walk.position
      var found = false
      // scroll to the end of the block
      while (!found && walk.hasCurrent) {
        scrollToTerm(walk)
        walk.skipBlankLines()
        if (walk.atSpaces(blockIndent)) {
          val i = walk.position
          walk.skip(blockIndent)
          if (!lookingAtOlMarker(walk) && !walk.atSpace) {
            found = true
            walk.startFrom(i)
          }
        } else found = true
      }
      // we got OL region, process it
      val ol = stripSelector(new SubSeqWalker(walk, startIdx, walk.position))
      processOl(ol)
      true
    } else false
  }

  def lookingAtOlMarker(walk: Walker): Boolean = {
    if (!walk.atDigit) return false
    walk.lookahead { w =>
      while (w.atDigit)
        w.skip()
      w.at(". ")
    }
  }

  def processOl(walk: Walker) {
    assert(walk.at("1. "))
    out.write("<ol")
    selector.writeAttrs(out, walk.getAbsolutePosition)
    out.write(">")
    // determining the bounds of each li
    // skip the marker, must point to the start of the first li
    while (walk.atDigit)
      walk.skip()
    walk.skip(2)
    var startIdx = walk.position
    while (walk.hasCurrent) {
      if (walk.atNewLine &&
          walk.skipBlankLines().atSpaces(blockIndent) &&
          lookingAtOlMarker(walk.skip(blockIndent))) {
        val li = stripSelector(new SubSeqWalker(walk, startIdx, walk.position))
        processLi(li)
        // skip next marker
        while (walk.atDigit)
          walk.skip()
        walk.skip(2)
        startIdx = walk.position
      } else walk.skip()
    }
    // flush last li
    val li = stripSelector(new SubSeqWalker(walk, startIdx, walk.position))
    processLi(li)
    // all li are done
    out.write("</ol>")
  }

  def processLi(walk: Walker) {
    out.write("<li")
    selector.writeAttrs(out, walk.getAbsolutePosition)
    out.write(">")
    // determine, whether the content is inline or block
    val b = walk.lookahead { w =>
      scrollToTerm(w)
      w.hasCurrent   // in other words, there is double line inside
    }
    val indent = blockIndent
    if (b) {
      while(walk.hasCurrent)
        block(walk)
      blockIndent = indent
    } else inline.process(walk)
    out.write("</li>")
  }

  /*! Headings are simple: they start with '#', the amound of pounds
  designates the level of the heading. */
  def tryHeading(walk: Walker): Boolean = {
    if (walk.at("#")) {
      val oldPos = walk.position
      var level = 0
      while (walk.at("#")) {
        walk.skip()
        level += 1
      }
      if (walk.at(" ")) {
        walk.skip()
        val startIdx = walk.position
        scrollToTerm(walk)
        val h = stripSelector(new SubSeqWalker(walk, startIdx, walk.position))
        out.write("<h")
        out.write(level.toString)
        selector.writeAttrs(out, walk.getAbsoluteIndex(oldPos))
        out.write(">")
        inline.process(h)
        out.write("</h")
        out.write(level.toString)
        out.write(">")
        true
      } else {
        walk.startFrom(oldPos)
        false
      }
    } else false
  }

  /*! HRs and tables have one thing in common: they start with `---`.
  They are processed together to decrease an impact on matching. */
  def tryHrTable(walk: Walker): Boolean = {
    if (walk.at("---")) {
      val oldPos = walk.position
      // walk towards the end of block
      scrollToTerm(walk)
      val b = stripSelector(new SubSeqWalker(walk, oldPos, walk.position))
      processHrTable(b)
      true
    } else false
  }

  def processHrTable(walk: Walker) {
    assert(walk.at("---"))
    if (!walk.atMatch(const.hr).isEmpty) {
      out.write("<hr")
      selector.writeAttrs(out, walk.getAbsolutePosition)
      out.write("/>")
    } else if (!walk.atMatch(const.table).isEmpty) {
      processTable(walk)
    } else processParagraph(walk, walk.getAbsolutePosition)
  }

  def processTable(walk: Walker) {
    assert(walk.at("---"))
    out.write("<table")
    selector.writeAttrs(out, walk.getAbsolutePosition)
    // scan for width attribute by walking the first sequence of `---`
    var widthAttr = ""
    while (walk.at("-")) walk.skip()
    if (walk.at(">")) {
      widthAttr = " width=\"100%\""
      walk.skip()
    }
    out.write(widthAttr)
    out.write(">")
    // now determine the column count by reading the first row
    val cells = readCells(walk)
    val cols = cells.size
    var alignAttrs: Seq[String] = Nil
    var hasHead = false
    // scan the next line for align data, if it fits the description of
    // the separator
    walk.at(const.tableSeparatorLine) match {
      case Some(m) =>
        hasHead = true
        alignAttrs ++= readCells(new SubSeqWalker(walk, m.start, m.end)) map { w =>
          val m = w.toString.trim
          val left = m.startsWith(":")
          val right = m.endsWith(":")
          if (left && right) " style=\"text-align:center\""
          else if (left) " style=\"text-align:left\""
          else if (right) " style=\"text-align:right\""
          else ""
        }
        walk.startFrom(m.end)
      case _ =>
    }
    if (hasHead) {
      out.write("<thead>")
      writeRow("th", cells, alignAttrs)
      out.write("</thead>")
    }
    // now write body
    out.write("<tbody>")
    var found = false
    if (!hasHead)  // don't forget that first row
      writeRow("td", cells, alignAttrs)
    while (!found && walk.hasCurrent) {
      if (walk.at(const.tableEndLine).isDefined)
        found = true
      else
        writeRow("td",
          readCells(walk).take(cols).padTo(cols, new SubSeqWalker("")),
          alignAttrs)
    }
    out.write("</tbody>")
    out.write("</table>")
  }

  def writeRow(tag: String, cells: Seq[Walker], alignAttrs: Seq[String]) {
    out.write("<tr>")
    var i = 0
    val attrs = alignAttrs.take(cells.size).padTo(cells.size, "")
    cells.foreach { w =>
      out.write("<")
      out.write(tag)
      out.write(attrs(i))
      out.write(">")
      inline.process(w)
      out.write("</")
      out.write(tag)
      out.write(">")
      i += 1
    }
    out.write("</tr>")
  }

  def readCells(walk: Walker): Seq[Walker] = {
    val buffer = new ListBuffer[Walker]
    // skip leading whitespace and |, respect \|
    walk.skipWhitespaces()
    if (walk.at("|")) walk.skip()
    var i = walk.position
    while (walk.hasCurrent && !walk.atNewLine) {
      if (walk.at("\\|")) walk.skip(2)
      if (walk.at("|")) {
        buffer += new SubSeqWalker(walk, i, walk.position)
        walk.skip()
        i = walk.position
      } else walk.skip()
    }
    // flush the last cell, if it's not empty
    val w = new SubSeqWalker(walk, i, walk.position)
    if (w.atMatch(const.empty).isEmpty)
      buffer += w
    // move the walker to the next line
    walk.skipNewLine()
    buffer
  }

  /*! HTML fragments are left "as is" without further transformation. Selectors
  are not applicable. */
  def tryHtml(walk: Walker): Boolean = {
    if (walk.at("<")) {
      // try to match it with the tag
      walk.at(const.htmlTag) match {
        case Some(m) =>
          val tag = m.group(0)
          val tagName = m.group(1)
          if (!const.blockTags.contains(tagName)) {
            // this tag is not recognized as block-level, so the whole block will be
            // processed like paragraph starting with inline element
            return false
          }
          if (tag.startsWith("</") || m.group(2) != null) {
            // closing and self-closing are uninteresting, spit them out
            out.write(tag)
            walk.startFrom(m.end)
          } else {
            // search for the matching closing tag, recursively
            val startIdx = walk.position
            walk.startFrom(m.end)
            scrollToClosingTag(walk, tagName)
            val w = new SubSeqWalker(walk, startIdx, walk.position)
            while (w.hasCurrent)
              inline.flushPlain(w)
          }
          true
        case _ => // try html comment as well
          walk.at(const.htmlComment) match {
            case Some(m) =>
              out.write(m.group(0))
              walk.startFrom(m.end)
              true
            case _ =>
              false
          }
      }
    } else false
  }

  def scrollToClosingTag(walk: Walker, tagName: String) {
    var found = false
    while (!found && walk.hasCurrent) {
      if (walk.at("<")) {
        walk.at(const.htmlTag) match {
          case Some(m) =>
            val name = m.group(1)
            walk.startFrom(m.end)
            // we only care about tags matching `tagName` and not self-closing ones
            if (name == tagName && m.group(2) == null) {
              val tag = m.group(0)
              if (tag.startsWith("</")) found = true
              else {  // tag is opening, search recursively
                scrollToClosingTag(walk, tagName)
              }
            }
          case _ =>
            walk.skip()
        }
      } else walk.skip()
    }
  }

  /*! To match div- and code-blocks we should be able to
  find their closing markers, respecting backslash escapes.*/
  def scrollToMarker(walk: Walker, marker: String) {
    var found = false
    while(!found && walk.hasCurrent) {
      if (walk.at("\\" + marker))
        walk.skip(marker.length + 1)
      if (walk.at(marker)) found = true
      else walk.skip()
    }
  }

  def tryFragmentBlock(walk: Walker): Boolean =
    if (walk.at("{{{")) {
      val startIdx = walk.position
      scrollToTerm(walk)
      val p = stripSelector(new SubSeqWalker(walk, startIdx, walk.position))
      p.atMatch(const.fragmentBlock)
          .flatMap(m => conf.resolveFragment(m.group(1)))
          .map { fragDef =>
        out.write("<div")
        selector.writeAttrs(out, startIdx)
        out.write(">")
        fragDef.mode match {
          case ProcessingMode.PLAIN => // like triple code span
            val w = new SubSeqWalker(fragDef.body)
            while (w.hasCurrent)
              inline.flushPlain(w)
          case ProcessingMode.CODE => // like regular code span
            val w = new SubSeqWalker(fragDef.body)
            while (w.hasCurrent)
              inline.flushCode(w)
          case _ => // like regular blocks
            run(new SubSeqWalker(fragDef.body))
        }
        out.write("</div>")
        true
      } getOrElse {
        walk.startFrom(startIdx)
        false
      }
    } else false

  def tryCodeBlock(walk: Walker): Boolean = {
    if (walk.at("```")) {
      val oldPos = walk.position
      walk.skip(3)
      val startIdx = walk.position
      scrollToMarker(walk, "```")
      val b = stripSelector(new SubSeqWalker(walk, startIdx, walk.position))
      out.write("<pre")
      selector.writeAttrs(out, walk.getAbsoluteIndex(oldPos))
      out.write("><code>")
      processCodeBlock(b)
      out.write("</code></pre>")
      walk.skip(3).skipBlankLines()
      true
    } else false
  }

  def processCodeBlock(walk: Walker) {
    // process line by line, removing the indent of the first line
    walk.skipBlankLines()
    countBlockIndent(walk)
    var startIdx = walk.position
    while (walk.hasCurrent) {
      if (walk.atNewLine) { // got a line, process it
        walk.skipNewLine()
        val line = new SubSeqWalker(walk, startIdx, walk.position)
        while (line.hasNext)
          inline.flushCode(line)
        out.write("\n")
        // reposition to next line, removing the indent
        if (walk.atSpaces(blockIndent)) walk.skip(blockIndent)
        startIdx = walk.position
      } else walk.skip()
    }
  }

  def tryDiv(walk: Walker): Boolean = {
    if (walk.at("~~~")) {
      val oldPos = walk.position
      walk.skip(3)
      val startIdx = walk.position
      scrollToMarker(walk, "~~~")
      val b = stripSelector(new SubSeqWalker(walk, startIdx, walk.position))
      out.write("<div")
      selector.writeAttrs(out, walk.getAbsoluteIndex(oldPos))
      out.write(">")
      while (b.hasCurrent) block(b)
      out.write("</div>")
      walk.skip(3).skipBlankLines()
      true
    } else false
  }

  def paragraph(walk: Walker) {
    walk.skipWhitespaces()
    if (walk.hasCurrent) {
      val startIdx = walk.position
      scrollToTerm(walk)
      val p = stripSelector(new SubSeqWalker(walk, startIdx, walk.position))
      processParagraph(p, startIdx)
    }
  }

  def processParagraph(walk: Walker, startIdx: Int) {
    out.write("<p")
    selector.writeAttrs(out, startIdx)
    out.write(">")
    inline.process(walk)
    out.write("</p>")
  }

}
