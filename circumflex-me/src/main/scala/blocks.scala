package ru.circumflex.me

import java.util.regex._

abstract class Block(val text: StringEx, val selector: Selector) {
  def element: String
  def toHtml(mp: MarkevenProcessor): StringEx = {
    val result = new StringEx(mp.currentIndent)
        .append("<")
        .append(element)
        .append(selector.toString)
    if (text.length == 0) result.append("/>")
    else result.append(">")
        .append(processContent(mp).buffer)
        .append("</")
        .append(element)
        .append(">")
    return result
  }
  def processContent(mp: MarkevenProcessor): StringEx = text
  def encodeUnsafeChars(s: StringEx): StringEx = s
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll("*", "&#42;")
      .replaceAll("`", "&#96;")
      .replaceAll("_", "&#95;")
      .replaceAll("\\", "&#92;")
}

abstract class NestedMarkupBlock(text: StringEx, selector: Selector)
    extends Block(text, selector) {
  def trimPattern: Option[Pattern]

  override def processContent(mp: MarkevenProcessor): StringEx = {
    // perform line trimming
    trimPattern.map(p => text.replaceAll(p, ""))
    // read nested blocks
    val blocks = mp.readBlocks(text)
    // do not wrap single paragraph
    if (blocks.size == 1 && blocks(0).isInstanceOf[ParagraphBlock])
      return blocks(0).processContent(mp)
    else return new StringEx("\n\n").append(mp.formHtml(blocks, true)).append(mp.currentIndent)
  }
}

abstract class ListBlock(text: StringEx, selector: Selector, val baseline: Int)
    extends NestedMarkupBlock(text, selector) {

  override def processContent(mp: MarkevenProcessor): StringEx = {
    // strip whitespace from every line if first element was indented
    if (baseline > 0)
      text.replaceAll(regexes.outdent(baseline), "")
    // read list item blocks
    val blocks = text.split(regexes.listItemSplit).map { s =>
      val selector = mp.stripSelector(s)
      val indent = trimPattern.map(p => s.replaceFirst(p, "")).getOrElse(0)
      createListItemBlock(s, selector, indent)
    }
    return new StringEx("\n\n").append(mp.formHtml(blocks, true)).append(mp.currentIndent)
  }

  def createListItemBlock(t: StringEx, s: Selector, indent: Int): Block =
    new ListItemBlock(t, s, indent)
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
}

class HeadingBlock(text: StringEx, selector: Selector, val level: Int)
    extends Block(text, selector) {
  def element = "h" + level
}

class CodeBlock(text: StringEx, selector: Selector)
    extends Block(text, selector) {
  def element = "code"
  override def toHtml(mp: MarkevenProcessor): StringEx = new StringEx(mp.currentIndent)
      .append("<pre")
      .append(selector.toString)
      .append("><code>")
      .append(processContent(mp).buffer)
      .append("</code></pre>")
}

class UnorderedListBlock(text: StringEx, selector: Selector, baseline: Int)
    extends ListBlock(text, selector, baseline) {
  def element = "ul"
  def trimPattern = Some(regexes.t_ul)
}

class OrderedListBlock(text: StringEx, selector: Selector, baseline: Int)
    extends ListBlock(text, selector, baseline) {
  def element = "ol"
  def trimPattern = Some(regexes.t_ol)
}

class DefinitionListBlock(text: StringEx, selector: Selector, baseline: Int)
    extends ListBlock(text, selector, baseline) {
  def element = "dl"
  def trimPattern = Some(regexes.t_dl)
}

class ListItemBlock(text: StringEx, selector: Selector, baseline: Int)
    extends NestedMarkupBlock(text, selector) {
  def trimPattern = if (baseline > 0) Some(regexes.outdent(baseline)) else None
  def element = "li"
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
  def element = "table"
}
