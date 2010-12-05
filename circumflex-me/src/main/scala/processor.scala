package ru.circumflex.me

import java.util.regex._
import collection.mutable.{HashMap, ListBuffer}
import ru.circumflex.core._


/*!# The Markeven Processor

`MarkevenProcessor` transforms text files into HTML using a set of simple rules.
It takes most ideas from [Markdown][], but has more strict rules, which lead to better
source structure and enhanced performance.

  [Markdown]: http://daringfireball.net/projects/markdown/syntax
*/
class MarkevenProcessor() {

  val protector = new Protector
  val links = new HashMap[String, LinkDefinition]()
  var level = 0
  val macros = new HashMap[String, CharSequence => CharSequence]()

  def increaseIndent: Unit = level += 1
  def decreaseIndent: Unit = if (level > 0) level -= 1

  def addMacro(name: String, function: CharSequence => CharSequence): this.type = {
    macros += (name -> function)
    return this
  }

  def currentIndent: String =
    if (level <= 0) return ""
    else "  " * level

  def normalize(s: StringEx): StringEx = s.replaceAll("\t","    ")
      .replaceAll(regexes.lineEnds, "\n")

  def cleanEmptyLines(s: StringEx): StringEx = s.replaceAll(regexes.blankLines, "")

  def stripLinkDefinitions(s: StringEx): StringEx = s.replaceAll(regexes.linkDefinition, m => {
    val id = m.group(1).trim.toLowerCase
    val url = new StringEx(m.group(2))
    var t = m.group(4)
    val title = new StringEx(if (t == null) "" else t)
    encodeChars(title)
    encodeBackslashEscapes(title)
    encodeChars(url)
    encodeBackslashEscapes(url)
    links += id -> new LinkDefinition(url, title)
    ""
  })

  def hashInlineHtml(s: StringEx, pattern: Pattern, out: String => String): StringEx =
    s.replaceIndexed(pattern, m => {
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
      val key = protector.addToken(s.buffer.subSequence(startIdx, endIdx))
      (out(key), endIdx)
    })

  def hashHtmlBlocks(s: StringEx): StringEx =
    hashInlineHtml(s, regexes.inlineHtmlBlockStart, key => "\n\n" + key + "\n\n")

  def hashHtmlComments(s: StringEx): StringEx = s.replaceAll(regexes.htmlComment, m =>
    "\n\n" + protector.addToken(m.group(0)) + "\n\n")

  def readBlocks(s: StringEx): Seq[Block] = {
    val result = new ListBuffer[Block]()
    val chunks = new ChunkIterator(s.split(regexes.blocks))
    while (chunks.hasNext)
      result += readBlock(chunks)
    return result
  }

  def readBlock(chunks: ChunkIterator): Block = {
    // get current chunk
    val s = chunks.next
    // strip selector if any
    val selector = stripSelector(s)
    // assume hashed inline HTML
    if (s.buffer.length == keySize + 2 && s.buffer.charAt(0) == '!' && s.buffer.charAt(1) == '}')
      protector.decode(s.buffer.toString) match {
        case Some(content) => return new InlineHtmlBlock(new StringEx(content))
        case _ => return new ParagraphBlock(s, selector)
      }
    // assume code block
    if (s.matches(regexes.d_code))
      return processComplexChunk(chunks, new CodeBlock(s, selector), c => c.matches(regexes.d_code))
    // trim any leading whitespace
    val indent = s.trimLeft
    // do not include empty freaks
    if (s.length == 0) return EmptyBlock
    // assume unordered list and ordered list
    if (s.startsWith("* "))
      return processComplexChunk(chunks, new UnorderedListBlock(s, selector, indent), c => {
        c.startsWith("* ") || c.startsWith(" ")
      })
    if (s.startsWith("1. "))
      return processComplexChunk(chunks, new OrderedListBlock(s, selector, indent), c => {
        c.startsWith(" ") || c.matches(regexes.d_ol)
      })
    // assume blockquote and section
    if (s.startsWith("> ")) return new BlockquoteBlock(s, selector)
    if (s.startsWith("| ")) return new SectionBlock(s, selector)
    // assume table, headings and hrs
    s.matches(regexes.d_table, m => {
      new TableBlock(s, selector)
    }) orElse s.matches(regexes.d_heading, m => {
      val marker = m.group(1)
      val body = m.group(2)
      new HeadingBlock(new StringEx(body), selector, marker.length)
    }) orElse s.matches(regexes.d_h1, m => {
      new HeadingBlock(new StringEx(m.group(1)), selector, 1)
    }) orElse s.matches(regexes.d_h2, m => {
      new HeadingBlock(new StringEx(m.group(1)), selector, 2)
    }) orElse s.matches(regexes.d_hr, m => {
      new HorizontalRulerBlock(selector)
    }) match {
      case Some(block: Block) => block
      case _ => // nothing matched -- paragraph
        new ParagraphBlock(s, selector)
    }
  }

  def processComplexChunk(chunks: ChunkIterator,
                          block: Block,
                          accept: StringEx => Boolean): Block = {
    var eob = false
    while (chunks.hasNext && !eob) {
      val c = chunks.peek
      if (accept(c)) {
        block.text.append("\n\n").append(c.buffer)
        chunks.next
      } else eob = true
    }
    return block
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

  def process(cs: CharSequence): StringEx = {
    val s = new StringEx(cs)
    normalize(s)
    stripLinkDefinitions(s)
    hashHtmlBlocks(s)
    hashHtmlComments(s)
    cleanEmptyLines(s)
    val blocks = readBlocks(s)
    return formHtml(blocks)
  }

  def transform(s: StringEx): StringEx = {
    protector.clear
    normalizeSpan(s)
    hashInlineHtml(s, regexes.inlineHtmlSpanStart, key => key)
    doMacros(s)
    encodeChars(s)
    doCodeSpans(s)
    encodeBackslashEscapes(s)
    doRefLinks(s)
    doInlineLinks(s)
    doEmphasis(s)
    doStrong(s)
    doDel(s)
    return unprotect(s)
  }

  def normalizeSpan(s: StringEx): StringEx =
    s.trim.replaceAll("  \n", "<br/>\n").replaceAll("\n", " ")

  def encodeChars(s: StringEx): StringEx =
    s.replaceAll(regexes.e_amp, "&amp;")
        .replaceAll("<", "&lt;")
        .replaceAll(">", "&gt;")

  def doMacros(s: StringEx): StringEx = s.replaceAll(regexes.macro, m => {
    var name = m.group(1)
    if (name == null) name = ""
    if (name.length > 0)
      name = name.substring(0, name.length - 1)
    val contents = m.group(2)
    val replacement = macros.get(name).map(f => f(contents)).getOrElse(
      "<span class=\"" + name + "\">" + contents + "</span>")
    protector.addToken(replacement)
  })

  def doCodeSpans(s: StringEx): Unit = s.replaceAll(regexes.codeSpan, m => {
    val s = new StringEx(m.group(2)).trim
    // there can be protected content inside codespans, so decode them first
    unprotect(s)
    encodeChars(s)
    protector.addToken(s.append("</code>").prepend("<code>"))
  })

  def encodeBackslashEscapes(s: StringEx): StringEx = s.replaceAll(regexes.backslashChar, m => {
    val c = m.group(0)
    escapeMap.getOrElse(c, c)
  })

  def doRefLinks(s: StringEx): StringEx = s.replaceAll(regexes.refLinks, m => {
    val linkText = m.group(1)
    var id = m.group(2).trim.toLowerCase
    if (id == "") id = linkText
    links.get(id).map(ld => ld.toLink(linkText)).getOrElse(m.group(0))
  })

  def doInlineLinks(s: StringEx): StringEx = s.replaceAll(regexes.inlineLinks, m => {
    val linkText = m.group(1)
    val url = m.group(2)
    var title = m.group(4)
    if (title == null) title = ""
    new LinkDefinition(new StringEx(url), new StringEx(title)).toLink(linkText)
  })

  def doEmphasis(s: StringEx): StringEx = s.replaceAll(regexes.emphasis, m =>
    "<em>" + m.group(1) + "</em>")

  def doStrong(s: StringEx): StringEx = s.replaceAll(regexes.strong, m =>
    "<strong>" + m.group(1) + "</strong>")

  def doDel(s: StringEx): StringEx = s.replaceAll(regexes.del, m =>
    "<del>" + m.group(1) + "</del>")

  def unprotect(s: StringEx): StringEx = s.replaceAll(regexes.protectKey, m => {
    val key = m.group(0)
    protector.decode(key).getOrElse(key)
  })

  def formHtml(blocks: Seq[Block], indent: Boolean = false): StringEx = {
    val result = new StringEx("")
    if (indent) level += 1
    blocks.foreach(b =>
      if (b != EmptyBlock) result.append(b.toHtml(this).buffer).append(newLine))
    if (indent) level -= 1
    return result
  }

  def newLine: String = "\n"

  def toHtml(cs: CharSequence): String = process(cs).toString

}

