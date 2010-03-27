package ru.circumflex.md

import java.util.regex._
import java.util.Random

/* # The Markdown Processor */

/**
 * This utility converts a plain text written in [Markdown][1] into HTML fragment.
 * The typical usage is:
 *
 *     val md = Markdown(myMarkdownText)
 *     // do some stuff
 *
 *  [1]: http://daringfireball.net/projects/markdown/syntax "Markdown Syntax"
 */
object Markdown {

  /* ## Commons */

  val keySize = 20
  val chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val rnd = new Random
  val blockTags = "p" :: "div" :: "h1" :: "h2" :: "h3" :: "h4" :: "h5" :: "h6" ::
      "blockquote" :: "pre" :: "table" :: "dl" :: "ol" :: "ul" :: "script" ::
      "noscript" :: "form" :: "fieldset" :: "iframe" :: "math" :: "ins" :: "del" :: Nil

  /* ## Regex patterns */

  // standardize line endings
  val rLineEnds = Pattern.compile("\\r\\n|\\r")
  // strip out whitespaces in blank lines
  val rBlankLines = Pattern.compile("^ +$", Pattern.MULTILINE)
  // replace tabs with spaces
  val rTabs = Pattern.compile("\\t")
  // start of inline HTML block
  val rInlineHtmlStart = Pattern.compile("^<(" + blockTags.mkString("|") + ")\\b[^/>]*?>",
    Pattern.MULTILINE | Pattern.CASE_INSENSITIVE)
  // HTML comments
  val rHtmlComment = Pattern.compile("^ {0,3}<!--(.|\\n)*?-->(?=\\n+|\\Z)", Pattern.MULTILINE)
  // Link definitions
  val rLinkDefinition = Pattern.compile("^ {0,3}\\[(.+)\\]:" +
      " *\\n? *<?(\\S+)>? *\\n? *" +
      "(?:[\"('](.+?)[\")'])?" +
      "(?=\\n+|\\Z)", Pattern.MULTILINE)
  // Character escaping
  val rEscAmp = Pattern.compile("&(?!#?[xX]?(?:[0-9a-fA-F]+|\\w+);)")
  val rEscLt = Pattern.compile("<(?![a-z/?\\$!])")
  // Headers
  val rH1 = Pattern.compile("^ {0,3}(\\S.*)\\n=+(?=\\n+|\\Z)", Pattern.MULTILINE)
  val rH2 = Pattern.compile("^ {0,3}(\\S.*)\\n-+(?=\\n+|\\Z)", Pattern.MULTILINE)
  val rHeaders = Pattern.compile("^(#{1,6}) *(\\S.*?) *#?$", Pattern.MULTILINE)
  // Horizontal rulers
  val rHr = Pattern.compile("^ {0,3}(?:(?:\\* *){3,})|(?:(?:- *){3,})|(?:(?:_ *){3,}) *$",
    Pattern.MULTILINE)

  /* ## The `apply` method */

  /**
   * Converts the `source` from Markdown to HTML.
   */
  def apply(source: String): String = new MarkdownText(source).toHtml

}

/* # The Processing Stuff */

/**
 * We collect all processing logic within this class.
 */
class MarkdownText(source: CharSequence) {
  protected var text = new StringEx(source)
  import Markdown._

  /* ## Link Definitions */

  case class LinkDefinition(val url: String, val title: String) {
    override def toString = url + " (" + title + ")"
  }

  protected var links: Map[String, LinkDefinition] = Map()

  /* ## Protectors */

  val htmlProtector = new Protector
  val charProtector = new Protector

  /* ## Processing methods */

  /**
   * Normalization includes following stuff:
   *
   * * replace DOS- and Mac-specific line endings with `\n`;
   * * replace tabs with spaces;
   * * reduce all blank lines (i.e. lines containing only spaces) to empty strings.
   */
  protected def normalize() = {
    text.replaceAll(rLineEnds, "\n")
        .replaceAll(rTabs, "    ")
        .replaceAll(rBlankLines, "")
  }

  /**
   * Ampersands and less-than signes are encoded to `&amp;` and `&lt;` respectively.
   */
  protected def encodeAmpsAndLts() = {
    text.replaceAll(rEscAmp, "&amp;")
        .replaceAll(rEscLt, "&lt;")
  }

  /**
   * All inline HTML blocks are hashified, so that no harm is done to their internals.
   */
  protected def hashHtmlBlocks(): Unit = {
    val m = text.matcher(rInlineHtmlStart)
    if (m.find) {
      val tagName = m.group(1)
      // This regex will match either opening or closing tag;
      // opening tags will be captured by $1 leaving $2 empty,
      // while closing tags will be captured by $2 leaving $1 empty
      val mTags = text.matcher(Pattern.compile(
        "(<" + tagName + "\\b[^/>]*?>)|(</" + tagName + "\\s*>)",
        Pattern.CASE_INSENSITIVE))
      // Find end index of matching closing tag
      var depth = 1
      var idx = m.end
      while (depth > 0 && idx < text.length && mTags.find(idx)) {
        if (mTags.group(2) == null) depth += 1
        else depth -= 1
        idx = mTags.end
      }
      // Having inline HTML subsequence
      val endIdx = idx
      val startIdx = m.start
      text.protectSubseq(htmlProtector, startIdx, endIdx)
      // Continue recursively until all blocks are processes
      hashHtmlBlocks
    }
  }

  /**
   * All HTML comments are hashified too.
   */
  protected def hashHtmlComments(): Unit = {
    val m = text.matcher(rHtmlComment)
    if (m.find) {
      text.protectSubseq(htmlProtector, m.start, m.end)
      hashHtmlComments
    }
  }

  /**
   * Standalone link definitions are added to the dictionary and then
   * stripped from the document.
   */
  protected def stripLinkDefinitions() = text.replaceAll(rLinkDefinition, m => {
    val id = m.group(1).toLowerCase
    val url = m.group(2)
    val title = if (m.group(3) == null) "" else m.group(3)
    links += id -> LinkDefinition(url, title.replaceAll("\"", "&quot;"))
    ""
  })

  protected def processBlocks() {
    doHeaders()
    doHorizontalRulers()
  }

  protected def doHeaders() = {
    text.replaceAll(rH1, "<h1>$1</h1>")
        .replaceAll(rH2, "<h2>$1</h2>")
        .replaceAll(rHeaders, m => {
      val marker = m.group(1)
      val body = m.group(2)
      "<h" + marker.length + ">" + body + "</h" + marker.length + ">"
    })
  }

  protected def doHorizontalRulers() = {
    text.replaceAll(rHr, "<hr/>")
  }

  /**
   * Transforms the Markdown source into HTML.
   */
  def toHtml(): String = {
    normalize()
    hashHtmlBlocks()
    hashHtmlComments()
    encodeAmpsAndLts()
    stripLinkDefinitions()
    processBlocks()
    println(text)
    println(htmlProtector)
    println(charProtector)
    println(links)
    ""
  }

}
