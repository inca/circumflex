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

class ChunkIterator(val chunks: Seq[StringEx]) {
  private var index = -1
  def hasNext: Boolean = (index + 1) < chunks.length
  def next: StringEx = {
    index += 1
    return chunks(index)
  }
  def peek: StringEx = chunks(index + 1)
  def reset: this.type = {
    index = -1
    return this
  }
}

class MarkevenProcessor(val ctx: MarkevenContext = new MarkevenContext) {

  def normalize(s: StringEx): StringEx = s.replaceAll("\t","    ")
      .replaceAll(regexes.lineEnds, "\n")

  def cleanEmptyLines(s: StringEx): StringEx = s.replaceAll(regexes.blankLines, "")

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
    ("\n\n" + key + "\n\n", endIdx)
  })

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
      ctx.protector.decode(s.buffer.toString) match {
        case Some(content) => return new InlineHtmlBlock(new StringEx(content))
        case _ => return new ParagraphBlock(s, selector)
      }
    // assume code block
    if (s.matches(regexes.d_code))
      return processComplexChunk(chunks, new CodeBlock(s, selector), c => {
        c.matches(regexes.d_code)
      })
    // trim any leading whitespace
    s.trimLeft
    // assume unordered list, ordered list and definition list
    if (s.startsWith("* "))
      return processComplexChunk(chunks, new UnorderedListBlock(s, selector), c => {
        c.startsWith("* ") || c.startsWith(" ")
      })
    if (s.startsWith("1. "))
      return processComplexChunk(chunks, new OrderedListBlock(s, selector), c => {
        c.startsWith(" ") || c.matches(regexes.d_ol)
      })
    if (s.startsWith(": "))
      return processComplexChunk(chunks, new DefinitionListBlock(s, selector), c => {
        c.startsWith(" ") || c.startsWith(": ")
      })
    // assume blockquote and section
    if (s.startsWith("> ")) return new BlockquoteBlock(s, selector)
    if (s.startsWith("| ")) return new SectionBlock(s, selector)
    // assume heading and table
    if (s.matches(regexes.d_heading)) return new HeadingBlock(s, selector)
    if (s.matches(regexes.d_table)) return new TableBlock(s, selector)
    // assume hr
    if (s.matches(regexes.d_hr)) return new HorizontalRulerBlock(selector)
    // nothing matched -- paragraph
    return new ParagraphBlock(s, selector)
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
    cleanEmptyLines(s)
    val blocks = readBlocks(s)
    return formHtml(blocks) 
  }

  def formHtml(blocks: Seq[Block]): StringEx =
    blocks.foldLeft(new StringEx(""))((s, b) => s.append(b.toHtml(this).buffer).append("\n\n"))

  def toHtml(cs: CharSequence): String = process(cs).toString

}
