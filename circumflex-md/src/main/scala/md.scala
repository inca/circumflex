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

  /* ## The `apply` method */

  /**
   * Converts the `source` from Markdown to HTML.
   */
  def apply(source: String): String = new MarkdownText(source).toHtml

}

/* # The Processing Stuff */

/**
 * We collect all processing logic for the input characters within this class.
 * It is not supposed to play with the stuff inside, but just in case you decide to,
 * there's a couple of things you should know.
 */
class MarkdownText(source: CharSequence) {
  /**
   * The first one: `source` gets wrapped into `StringBuffer`. See, while in general
   * `StringBuilder` provides more speed, for some reason Java regular expressions
   * provide support only for buffers (that are organized in a thread-safe manner, which
   * tend to be much slower than builder). Since we use a single-thread model here, we
   * find that fact offensive to all the `StringBuilder` thang, but sincerely can't do
   * no damn thing about it.
   */
  protected var text = new StringBuffer(source)

  /**
   * The second one: we use the default `Markdown` singleton to lookup precompiled regexes.
   * I know it sounds like saving on buying bread, but we are really concerned about
   * the performance of that little thingy =)
   */
  import Markdown._

  /**
   * And the last one: as I mentioned before, we use a single-thread model here. The `toHtml`
   * method is supposed to be called only once per instance, and, since the `text` is mutable
   * calling it twice may cause *very* unpredictible results.
   *
   * Have fun! 
   */

  /* ## Link Definitions */

  case class LinkDefinition(val url: String, val title: String) {
    override def toString = url + " (" + title + ")"
  }

  protected var links: Map[String, LinkDefinition] = Map()

  /* ## Protectors and hashing */

  class Protector {
    protected var protectHash: Map[String, CharSequence] = Map()
    protected var unprotectHash: Map[CharSequence, String] = Map()

    /**
     * Generates a random hash key.
     */
    def randomKey = (0 to keySize).foldLeft("")((s, i) =>
      s + chars.charAt(rnd.nextInt(keySize)))

    /**
     * Adds the subsequence between `startIdx` and `endIdx` to hash and
     * applies a replacement to the `text`.
     */
    def addSubseq(startIdx: Int, endIdx: Int): String = {
      val subseq = text.subSequence(startIdx, endIdx)
      val key = addToken(subseq)
      text = new StringBuffer(text.subSequence(0, startIdx))
          .append(key)
          .append("\n\n")
          .append(text.subSequence(endIdx, text.length))
      return key
    }

    /**
     * Adds the specified token to hash and returns the protection key.
     */
    def addToken(t: CharSequence): String = {
      val key = randomKey
      protectHash += key -> t
      unprotectHash += t -> key
      return key
    }

    override def toString = protectHash.toString
  }

  val htmlProtector = new Protector
  val charProtector = new Protector

  /* ## Utility methods */

  /**
   * A convenient equivalent to `String.replaceAll` that accepts `Pattern`
   * instead of `String`.
   */
  protected def replaceAll(pattern: Pattern, replacement: String): Unit =
    replaceAll(pattern, m => replacement)

  /**
   * A convenient equivalent to `String.replaceAll` that accepts `Pattern`
   * instead of `String` and a function `Matcher => String`.
   */
  protected def replaceAll(pattern: Pattern, replacementFunction: Matcher => String): Unit = {
    val lastIndex = 0;
    val m = pattern.matcher(text);
    val sb = new StringBuffer();
    while (m.find()) m.appendReplacement(sb, replacementFunction(m));
    m.appendTail(sb);
    text = sb;
  }

  /**
   * Appends the specified character sequence.
   */
  protected def append(s: CharSequence) = {
    text.append(s)
  }

  /**
   * Prepends the specified character sequence.
   */
  protected def prepend(s: CharSequence) = {
    val sb = new StringBuffer(s).append(text)
    text = sb
  }

  /* ## Processing methods */

  /**
   * Normalization includes following stuff:
   *
   * * replace DOS- and Mac-specific line endings with `\n`;
   * * replace tabs with spaces;
   * * reduce all blank lines (i.e. lines containing only spaces) to empty strings.
   */
  protected def normalize() = {
    replaceAll(rLineEnds, "\n")
    replaceAll(rTabs, "    ")
    replaceAll(rBlankLines, "")
  }

  /**
   * Ampersands and less-than signes are encoded to `&amp;` and `&lt;` respectively.
   */
  protected def encodeAmpsAndLts() = {
    replaceAll(rEscAmp, "&amp;")
    replaceAll(rEscLt, "&lt;")
  }

  /**
   * All inline HTML blocks are hashified, so that no harm is done to their internals.
   */
  protected def hashHtmlBlocks(): Unit = {
    val m = rInlineHtmlStart.matcher(text)
    if (m.find) {
      val tagName = m.group(1)
      // This regex will match either opening or closing tag;
      // opening tags will be captured by $1 leaving $2 empty,
      // while closing tags will be captured by $2 leaving $1 empty
      val mTags = Pattern.compile(
        "(<" + tagName + "\\b[^/>]*?>)|(</" + tagName + "\\s*>)",
        Pattern.CASE_INSENSITIVE).matcher(text)
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
      htmlProtector.addSubseq(startIdx, endIdx)
      // Continue recursively until all blocks are processes
      hashHtmlBlocks
    }
  }

  /**
   * All HTML blocks are hashified too.
   */
  protected def hashHtmlComments(): Unit = {
    val m = rHtmlComment.matcher(text)
    if (m.find) {
      htmlProtector.addSubseq(m.start, m.end)
      hashHtmlComments
    }
  }

  /**
   * Standalone link definitions are added to the dictionary and then
   * stripped from the document.
   */
  protected def stripLinkDefinitions() = replaceAll(rLinkDefinition, m => {
    val id = m.group(1).toLowerCase
    val url = m.group(2)
    val title = if (m.group(3) == null) "" else m.group(3)
    links += id -> LinkDefinition(url, title.replaceAll("\"", "&quot;"))
    ""
  })

  /**
   * Transforms the Markdown source into HTML.
   */
  def toHtml(): String = {
    normalize()
    hashHtmlBlocks()
    hashHtmlComments()
    encodeAmpsAndLts()
    stripLinkDefinitions()
    println(text)
    println(htmlProtector)
    println(charProtector)
    println(links)
    ""
  }

}
