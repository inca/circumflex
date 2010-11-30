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
  def trimPattern: Pattern

  def processNested(mp: MarkevenProcessor): StringEx = {
    // perform line trimming
    text.replaceAll(trimPattern, "")
  }
}

object EmptyBlock extends Block(new StringEx(""), new Selector) {
  def element = ""
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

class UnorderedListBlock(text: StringEx, selector: Selector) extends Block(text, selector) {
  def element = "ul"
}

class OrderedListBlock(text: StringEx, selector: Selector) extends Block(text, selector) {
  def element = "ol"
}

class DefinitionListBlock(text: StringEx, selector: Selector) extends Block(text, selector) {
  def element = "dl"
}

class BlockquoteBlock(text: StringEx, selector: Selector) extends Block(text, selector) {
  def element = "blockquote"
}

class SectionBlock(text: StringEx, selector: Selector) extends Block(text, selector) {
  def element = "div"
}

class TableBlock(text: StringEx, selector: Selector) extends Block(text, selector) {
  def element = "table"
}
