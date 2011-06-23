package ru.circumflex.markeven

import collection.mutable.{HashMap}
import java.io.{Writer, StringWriter}

/*!# Markeven Worker

`MarkevenWorker` is a class which performs actual processing. It should be created for each processing task
and should never be shared by different threads or tasks.
*/
class MarkevenWorker(conf: MarkevenConfiguration) {
  protected val protector = new Protector
  protected val linkDefs = new HashMap[String, LinkDefinition]

  def resolveLink(id: String): Option[LinkDefinition] = linkDefs.get(id).orElse(conf.resolveLink(id))
  def processUrl(url: CharSequence): StringEx = new StringEx(url)

  protected var level = 0

  def increaseIndent: Unit = level += 1
  def decreaseIndent: Unit = if (level > 0) level -= 1

  def currentIndent: String =
    if (level <= 0) return ""
    else conf.indent * level

  /*! The `toHtml` method is a main entry point for processing semantic texts into HTML.
      You can also write output directly into any `java.io.Writer` using the `process` method.
   */

  def toHtml(cs: CharSequence): String = {
    val out = new StringWriter(cs.length)
    process(cs, out)
    return out.toString
  }

  def process(cs: CharSequence, out: Writer): Unit = {
    val s = new StringEx(cs)
    normalize(s)
    hashHtmlBlocks(s)
    hashHtmlComments(s)
    stripLinkDefinitions(s)
    writeHtml(readBlocks(s), out)
  }

  def writeHtml(blocks: Seq[Block], out: Writer): Unit =
    blocks.foreach(b => if (b != EmptyBlock) {
      b.writeHtml(this, out)
      out.write("\n")
    })

  /*!## Preprocessing

  Markeven will always operate on blocks. Blocks are delimited by one or more empty lines.
  An empty line is a line containing zero or more whitespace characters.

  The only exception to this rule are HTML blocks and comments: they remain untouched.
  Markeven does its best to find a proper match for HTML tags, but, of course, it can't
  magically "cure" broken markup.

  Lines are also _normalized_ before processing, that is trailing whitespace are removed
  from each line and line ending characters are standardized (we prefer Unix way: `\n`).

  Tabs are also replaced with four spaces during normalization.

  Note that normalization is run in _all_ blocks, including unprocessed inline HTML.

  ## Link definitions

  Link definitions must contain on separate lines. They are stripped from the source
  right after normalization.
  */

  def normalize(s: StringEx): Unit = s.replaceAll("\t", "    ")
      .replaceAll(regexes.lineEnds, "\n")
      .replaceAll(regexes.blankLines, "")

  def stripLinkDefinitions(s: StringEx): Unit = s.replaceAll(regexes.linkDefinition, m => {
    val id = m.group(1).trim.toLowerCase
    val url = processUrl(m.group(2))
    var t = m.group(4)
    val title = new StringEx(if (t == null) "" else t)
    encodeChars(title)
    encodeBackslashEscapes(title)
    encodeChars(url)
    encodeBackslashEscapes(url)
    linkDefs += id -> new LinkDefinition(url, title)
    ""
  })

  def hashHtmlBlocks(s: StringEx): Unit =
    s.replaceIndexed(regexes.inlineHtmlBlockStart, m => {
      val startIdx = m.start
      var endIdx = 0
      if (m.group(2) != null) {
        // self-closing tag, escape as is
        endIdx = m.end
      } else {
        // find end-index of matching closing tag
        val tagName = m.group(1)
        // following regex will have `group(1) == null` for closing tags;
        // `group(2)` determines if a tag is self-closing.
        val tm = regexes.htmlTag(tagName).matcher(s.buffer)
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
      ("\n\n" + key + "\n\n", endIdx)
    })

  def hashHtmlComments(s: StringEx): Unit = s.replaceAll(regexes.htmlComment, m =>
    "\n\n" + protector.addToken(m.group(0)) + "\n\n")

  /*!## Blocks reading

  As mentioned above, Markeven will interpret blocks separated by two or more blank lines.
  */

  def readBlocks(s: StringEx): Seq[Block] = Nil






}