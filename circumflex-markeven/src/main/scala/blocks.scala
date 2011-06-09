package ru.circumflex.markeven

import java.io._
import java.util.regex._

class LinkDefinition(val url: StringEx, val title: StringEx) {
  url.replaceAll("*", "&#42;").replaceAll("_", "&#95;").trim
  title.replaceAll("*", "&#42;").replaceAll("_", "&#95;").replaceAll("\"", "&quot;").trim

  def toLink(linkText: CharSequence): StringEx = {
    val result = new StringEx("<a href=\"").append(url).append("\"")
    if (title.length > 0) result.append(" title=\"").append(title).append("\"")
    result.append(">").append(linkText).append("</a>")
    return result
  }

  def toImageLink(alt: CharSequence): StringEx = {
    val result = new StringEx("<img src=\"").append(url).append("\"")
    if (title.length > 0) result.append(" title=\"").append(title).append("\"")
    if (alt.length > 0) result.append(" alt=\"").append(alt).append("\"")
    result.append("/>")
    return result
  }
}

class Selector(val id: String = "", val classes: Seq[String] = Nil) {
  override val toString = {
    var result = ""
    if (id != "") result += " id=\"" + id + "\""
    if (classes.size > 0)
      result += " class=\"" + classes.mkString(" ") + "\""
    result
  }
}

abstract class Block(val text: StringEx, val selector: Selector) {
  def element: String
  def toHtml(mp: MarkevenProcessor): StringEx = {
    val content = processContent(mp)
    val result = new StringEx(mp.currentIndent)
        .append("<")
        .append(element)
        .append(selector.toString)
        .append(attributes)
    if (text.length == 0) result.append("/>")
    else result.append(">")
        .append(content)
        .append("</")
        .append(element)
        .append(">")
    return result
  }
  def writeHtml(mp: MarkevenProcessor, out: Writer): Unit =
    out.write(toHtml(mp).toString)
  def processContent(mp: MarkevenProcessor): StringEx = text
  def attributes = ""
}

abstract class NestedMarkupBlock(text: StringEx, selector: Selector)
    extends Block(text, selector) {
  def trimPattern: Option[Pattern]

  override def processContent(mp: MarkevenProcessor): StringEx = {
    // perform line trimming
    trimPattern.map(p => text.replaceAll(p, ""))
    // clean blank lines
    mp.cleanEmptyLines(text)
    // read nested blocks
    val blocks = mp.readBlocks(text)
    // do not wrap single paragraph
    if (blocks.size == 1 && unwrapBlock_?(blocks(0)))
      return blocks(0).processContent(mp)
    else return new StringEx(mp.newLine).append(mp.formHtml(blocks, true)).append(mp.currentIndent)
  }

  def unwrapBlock_?(block: Block): Boolean = block.isInstanceOf[ParagraphBlock]
}

abstract class ListBlock(text: StringEx, selector: Selector, val baseline: Int)
    extends NestedMarkupBlock(text, selector) {

  def splitPattern: Pattern

  override def processContent(mp: MarkevenProcessor): StringEx = {
    // strip whitespace from every line if first element was indented
    if (baseline > 0)
      text.replaceAll(regexes.outdent(baseline), "")
    // clean blank lines
    mp.cleanEmptyLines(text)
    // read list item blocks
    val blocks = text.split(splitPattern).map { s =>
      val selector = mp.stripSelector(s)
      val indent = trimPattern.map(p => s.replaceFirst(p, "")).getOrElse(0)
      if (indent > 0) s.replaceAll(regexes.outdent(indent), "")
      new ListItemBlock(s, selector)
    }
    return new StringEx(mp.newLine).append(mp.formHtml(blocks, true)).append(mp.currentIndent)
  }

}

object EmptyBlock extends Block(new StringEx(""), new Selector()) {
  def element = ""
  override def processContent(mp: MarkevenProcessor) = text
  override def toHtml(mp: MarkevenProcessor) = text
  override def writeHtml(mp: MarkevenProcessor, out: Writer): Unit = {}
}

class InlineHtmlBlock(text: StringEx)
    extends Block(text, new Selector) {
  def element = ""
  override def toHtml(mp: MarkevenProcessor): StringEx = text
}

class HorizontalRulerBlock(selector: Selector)
    extends Block(new StringEx(""), selector) {
  def element = "hr"
}

class ParagraphBlock(text: StringEx, selector: Selector)
    extends Block(text, selector) {
  def element = "p"
  override def processContent(mp: MarkevenProcessor): StringEx = mp.transform(text)
}

class HeadingBlock(text: StringEx, selector: Selector, val level: Int)
    extends Block(text, selector) {
  def element = "h" + level
  override def processContent(mp: MarkevenProcessor): StringEx = mp.transform(text)
}

class CodeBlock(text: StringEx, selector: Selector)
    extends Block(text, selector) {
  protected var _fenced = false
  def indented(): this.type = {
    _fenced = false
    return this
  }
  def fenced(): this.type = {
    _fenced = true
    return this
  }
  def element = "code"
  override def toHtml(mp: MarkevenProcessor): StringEx = new StringEx(mp.currentIndent)
      .append("<pre")
      .append(selector.toString)
      .append("><code>")
      .append(processContent(mp).buffer)
      .append("</code></pre>")
  override def processContent(mp: MarkevenProcessor): StringEx = {
    var result = text
    if (!_fenced)
      result = result.replaceAll(regexes.outdent(4), "")
    return mp.encodeChars(result)
  }
}

class UnorderedListBlock(text: StringEx, selector: Selector, baseline: Int)
    extends ListBlock(text, selector, baseline) {
  def element = "ul"
  def trimPattern = Some(regexes.t_ul)
  def splitPattern = regexes.s_ul
}

class OrderedListBlock(text: StringEx, selector: Selector, baseline: Int)
    extends ListBlock(text, selector, baseline) {
  def element = "ol"
  def trimPattern = Some(regexes.t_ol)
  def splitPattern = regexes.s_ol
}

class ListItemBlock(text: StringEx, selector: Selector)
    extends NestedMarkupBlock(text, selector) {
  def trimPattern = None
  def element = "li"
  override def unwrapBlock_?(block: Block): Boolean =
    block.isInstanceOf[ParagraphBlock] || block.isInstanceOf[SectionBlock]
}

class BlockquoteBlock(text: StringEx, selector: Selector)
    extends NestedMarkupBlock(text, selector) {
  def trimPattern = Some(regexes.t_blockquote)
  def element = "blockquote"
}

class SectionBlock(text: StringEx, selector: Selector)
    extends NestedMarkupBlock(text, selector) {
  def trimPattern = Some(regexes.t_div)
  def element = "div"
}

class TableBlock(text: StringEx, selector: Selector)
    extends Block(text, selector) {
  var widthAttr: String = ""
  def element = "table"
  override def attributes = widthAttr
  override def processContent(mp: MarkevenProcessor): StringEx = {
    val chunks = new ChunkIterator(text.split(regexes.lines))
    if (chunks.size < 3)    // no table content
      return new StringEx("")
    val result = new StringEx("")
    // first line determines width
    if (chunks.next.endsWith(">")) widthAttr = " width=\"100%\""
    // next line determines column count
    var cells = parseCells(mp, chunks.next)
    val cols = cells.size
    var align: Seq[String] = Nil
    val l = chunks.next
    // see if we got heading
    if (l.matches(regexes.tableSeparatorLine) && chunks.hasNext) {
      // we have a heading; but let's parse separator first
      align = l.split(regexes.tableCellSplit).map { a =>
        val v = a.trim
        if (v.startsWith(":") && v.endsWith(":")) " align=\"center\""
        else if (v.startsWith(":")) " align=\"left\""
        else if (v.endsWith(":")) " align=\"right\""
        else ""
      }
      // let's also flush heading
      mp.increaseIndent
      result.append(mp.newLine).append(mp.currentIndent).append("<thead>")
      mp.increaseIndent
      result.append(mp.newLine).append(mp.currentIndent).append("<tr>")
      mp.increaseIndent
      cells.foreach(th => result.append(mp.newLine).append(mp.currentIndent).append("<th>")
          .append(mp.transform(th)).append("</th>"))
      mp.decreaseIndent
      result.append(mp.newLine).append(mp.currentIndent).append("</tr>")
      mp.decreaseIndent
      result.append(mp.newLine).append(mp.currentIndent).append("</thead>")
      mp.decreaseIndent
      // read first body cells for correct positioning
      cells = parseCells(mp, chunks.next)
    } else chunks.stepBack
    // correct align data to match colsize
    align = align.take(cols).padTo(cols, "")
    // process body
    mp.increaseIndent
    result.append(mp.newLine).append(mp.currentIndent).append("<tbody>")
    mp.increaseIndent
    while (chunks.hasNext) {
      result.append(mp.newLine).append(mp.currentIndent).append("<tr>")
      mp.increaseIndent
      var i = 0
      cells.take(cols).padTo(cols, "").foreach { td =>
        result.append(mp.newLine).append(mp.currentIndent).append("<td")
            .append(align(i)).append(">").append(td).append("</td>")
        i += 1
      }
      mp.decreaseIndent
      result.append(mp.newLine).append(mp.currentIndent).append("</tr>")
      // evaluate next row
      val l = chunks.next
      if (chunks.hasNext)
        cells = parseCells(mp, l)
    }
    // now close body and we're done
    mp.decreaseIndent
    result.append(mp.newLine).append(mp.currentIndent).append("</tbody>")
    mp.decreaseIndent
    result.append(mp.newLine).append(mp.currentIndent)
    return result
  }

  def parseCells(mp: MarkevenProcessor, s: StringEx): Seq[StringEx] =
    s.replaceAll(regexes.t_tr, "")
        .split(regexes.tableCellSplit).map { s => mp.transform(s) }
}
