package ru.circumflex.me

import java.util.regex._
import collection.mutable.ListBuffer

class Selector(val id: String = "", val classes: Seq[String] = Nil) {
  override val toString = {
    var result = ""
    if (id != "") result += " id=\"" + id + "\""
    if (classes.size > 0)
      result += " class=\"" + classes.mkString(" ") + "\""
    result
  }
}

class LinkDefinition(val url: String, val title: String)

class MarkevenContext(var protector: Protector = new Protector,
                      var links: Map[String, LinkDefinition] = Map())

class MarkevenProcessor(val ctx: MarkevenContext = new MarkevenContext) {

  def normalize(s: StringEx): StringEx = s.replaceAll("\t","    ")
      .replaceAll(regexes.normalizeLines, "\n")

  def stripLinkDefinitions(s: StringEx): StringEx = s.replaceAll(regexes.linkDefinition, m => {
    val id = m.group(1).toLowerCase
    val url = m.group(2)
    var title = m.group(3)
    if (title != null) title = title.replace("\"", "&quot;")
    else title = ""
    ctx.links += id -> new LinkDefinition(url, title)
    ""
  })

  def hashHtmlBlocks(s: StringEx): StringEx = s.replaceIndexed(regexes.inlineHtmlStart, m => {
    var startIdx = m.start
    var endIdx = 0
    if (m.group(2) != null) {
      // self-closing tag, escape as is
      endIdx = m.end
    } else {
      // find end-index of matching closing tag
      val tagName = m.group(1)
      // following regex will have `group(1) == null` for closing tags;
      // `group(2)` determines if a tag is self-closing.
      val tm = Pattern.compile("(<" + tagName + "\\b.*?(/)?>)|(</" + tagName + "\\s*>)",
        Pattern.CASE_INSENSITIVE).matcher(s.buffer)
      var depth = 1
      var idx = m.end
      while (depth > 0 && idx < s.length && tm.find(idx)) {
        if (tm.group(1) == null) depth -= 1        // closing tag
        else if (tm.group(2) == null) depth += 1   // opening tag
        idx = tm.end
      }
      endIdx = idx
    }
    // add to protector and replace
    val key = ctx.protector.addToken(s.buffer.subSequence(startIdx, endIdx))
    ("\n" + key + "\n", endIdx)
  })

  def readBlocks(s: StringEx): Seq[Block] = s.split(regexes.blocks).map(s => parseBlock(s))

  def parseBlock(s: StringEx): Block = {
    // assume empty block
    if (s.length == 0) return EmptyBlock
    // strip selector if any
    val selector = stripSelector(s)
    // assume <hr/>
    if (s.buffer.length == 5 && s.buffer.toString == "* * *")
      return new HorizontalRulerBlock(selector)
    // assume hashed inline HTML
    if (s.buffer.length == keySize + 2 && s.buffer.charAt(0) == '!' && s.buffer.charAt(1) == '}')
      ctx.protector.decode(s.buffer.toString) match {
        case Some(content) => return new InlineHtmlBlock(new StringEx(content))
        case _ => return new ParagraphBlock(s, selector)
      }
    // assume blocks determined by their first char
    if (s.length > 2) {
      if (s.buffer.charAt(1) == ' ') {
        // assume unordered list
        if (s.buffer.charAt(0) == '*')
          return new UnorderedListBlock(s, selector)
        // assume definition list
        if (s.buffer.charAt(0) == ':')
          return new DefinitionListBlock(s, selector)
        // assume blockquote
        if (s.buffer.charAt(0) == '>')
          return new BlockquoteBlock(s, selector)
        // assume section
        if (s.buffer.charAt(0) == '|')
          return new SectionBlock(s, selector)
      }
      // assume ordered list
      if (regexes.d_ol.matcher(s.buffer).matches)
        return new OrderedListBlock(s, selector)
      // assume table
      if (regexes.d_table.matcher(s.buffer).matches)
        return new TableBlock(s, selector)
      // assume heading
      if (regexes.d_heading.matcher(s.buffer).matches)
        return new HeadingBlock(s, selector)
      // assume code block
      if (regexes.d_code.matcher(s.buffer).matches)
        return new CodeBlock(s, selector)
    }
    // nothing matched -- a paragraph is assumed
    return new ParagraphBlock(s, selector)
  }

  /**
   * Strips a selector from first line of block. Returns read selector.
   */
  def stripSelector(s: StringEx): Selector = {
    var id = ""
    var classes = new ListBuffer[String]()
    s.replaceAll(regexes.blockSelector, m => {
      val idSelector = m.group(1)
      val classesSelector = m.group(2)
      if (idSelector != null)
        id = idSelector.substring(1)
      if (classesSelector != null)
        classesSelector.split("\\.").foreach { cl =>
          if (cl != "")
            classes += cl
        }
      ""
    })
    return new Selector(id, classes)
  }

  def process(cs: CharSequence): String = {
    val s = new StringEx(cs)
    normalize(s)
    stripLinkDefinitions(s)
    hashHtmlBlocks(s)
    val blocks = readBlocks(s)
    blocks.map(b => b.getClass.getSimpleName + " ----> " + b.selector).mkString("\n")
  }

}

abstract class Block(val text: StringEx, val selector: Selector)

object EmptyBlock extends Block(new StringEx(""), new Selector)

class InlineHtmlBlock(text: StringEx) extends Block(text, new Selector)
class HorizontalRulerBlock(selector: Selector) extends Block(new StringEx(""), selector)
class ParagraphBlock(text: StringEx, selector: Selector) extends Block(text, selector)
class HeadingBlock(text: StringEx, selector: Selector) extends Block(text, selector)
class CodeBlock(text: StringEx, selector: Selector) extends Block(text, selector)
class UnorderedListBlock(text: StringEx, selector: Selector) extends Block(text, selector)
class OrderedListBlock(text: StringEx, selector: Selector) extends Block(text, selector)
class DefinitionListBlock(text: StringEx, selector: Selector) extends Block(text, selector)
class BlockquoteBlock(text: StringEx, selector: Selector) extends Block(text, selector)
class SectionBlock(text: StringEx, selector: Selector) extends Block(text, selector)
class TableBlock(text: StringEx, selector: Selector) extends Block(text, selector)


