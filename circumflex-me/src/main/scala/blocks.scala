package ru.circumflex.me

import java.util.regex._

abstract class Block(val text: StringEx, val selector: Selector) {
  def element: String
  def toHtml(mp: MarkevenProcessor): StringEx = {
    val result = new StringEx("<")
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
    else return mp.formHtml(blocks)
  }
}

abstract class ListBlock(text: StringEx, selector: Selector, val indent: Int)
    extends NestedMarkupBlock(text, selector) {

  override def processContent(mp: MarkevenProcessor): StringEx = {
    // strip whitespace from every line if first element was indented
    indent match {
      case 0 =>
      case 1 => text.replaceAll(regexes.t_space1, "")
      case 2 => text.replaceAll(regexes.t_space2, "")
      case 3 => text.replaceAll(regexes.t_space3, "")
      case _ => text.replaceAll(regexes.outdent, "") // should never happen, but...
    }
    // read list item blocks
    val blocks = text.split(regexes.listItemSplit).map { s =>
      val selector = mp.stripSelector(s)
      trimPattern.map(p => s.replaceAll(p, ""))
      createListItemBlock(s, selector)
    }
    return mp.formHtml(blocks)
  }

  def createListItemBlock(t: StringEx, s: Selector): Block = new ListItemBlock(t, s)
}

class InlineHtmlBlock(text: StringEx) extends Block(text, new Selector) {
  def element = ""
  override def toHtml(mp: MarkevenProcessor): StringEx = text
}

class HorizontalRulerBlock(selector: Selector) extends Block(new StringEx(""), selector) {
  def element = "hr"
}

class ParagraphBlock(text: StringEx, selector: Selector) extends Block(text, selector) {
  def element = "p"
}

class HeadingBlock(text: StringEx, selector: Selector) extends Block(text, selector) {
  def element = "h" + level
  def level: Int = 1
}

class CodeBlock(text: StringEx, selector: Selector) extends Block(text, selector) {
  def element = "code"
  override def toHtml(mp: MarkevenProcessor): StringEx = new StringEx("<pre")
      .append(selector.toString)
      .append("><code>")
      .append(processContent(mp).buffer)
      .append("</code></pre>")
}

class UnorderedListBlock(text: StringEx, selector: Selector, indent: Int)
    extends ListBlock(text, selector, indent) {
  def element = "ul"
  def trimPattern = Some(regexes.t_ul)
}

class OrderedListBlock(text: StringEx, selector: Selector, indent: Int)
    extends ListBlock(text, selector, indent) {
  def element = "ol"
  def trimPattern = Some(regexes.t_ol)
}

class DefinitionListBlock(text: StringEx, selector: Selector, indent: Int)
    extends ListBlock(text, selector, indent) {
  def element = "dl"
  def trimPattern = Some(regexes.t_dl)
}

class ListItemBlock(text: StringEx, selector: Selector) extends NestedMarkupBlock(text, selector) {
  def trimPattern = None
  def element = "li"
}

class BlockquoteBlock(text: StringEx, selector: Selector) extends NestedMarkupBlock(text, selector) {
  def trimPattern = Some(regexes.t_blockquote)
  def element = "blockquote"
}

class SectionBlock(text: StringEx, selector: Selector) extends NestedMarkupBlock(text, selector) {
  def trimPattern = Some(regexes.t_div)
  def element = "div"
}

class TableBlock(text: StringEx, selector: Selector) extends Block(text, selector) {
  def element = "table"
}
