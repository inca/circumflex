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

  /* ## Hashing */

  /**
   * Inlined HTML fragments are hashed so that the processing inside is skipped.
   */
  protected var htmlHash = Map[String, String]()

  /**
   * Generates a random hash key.
   */
  protected def randomKey = (0 to keySize).foldLeft("")((s, i) =>
    s + chars.charAt(rnd.nextInt(keySize)))

  /* ## Utility methods */

  /**
   * A convenient equivalent to `String.replaceAll` that accepts `Pattern`,
   * which is often compiled with `Pattern.MULTILINE`.
   */
  protected def replaceAll(pattern: Pattern, replacement: String): this.type = {
    val lastIndex = 0;
    val m = pattern.matcher(text);
    val sb = new StringBuffer();
    while (m.find()) m.appendReplacement(sb, replacement);
    m.appendTail(sb);
    text = sb;
    return this;
  }

  /**
   * Appends the specified character sequence.
   */
  protected def append(s: CharSequence): this.type = {
    text.append(s)
    return this
  }

  /**
   * Prepends the specified character sequence.
   */
  protected def prepend(s: CharSequence): this.type = {
    val sb = new StringBuffer(s).append(text)
    text = sb
    return this
  }

  /* ## Processing methods */

  /**
   * Normalization includes following stuff:
   *
   * * replace DOS- and Mac-specific line endings with `\n`;
   * * replace tabs with spaces;
   * * reduce all blank lines (i.e. lines containing only spaces) to empty strings.
   */
  protected def normalize(): this.type = replaceAll(rLineEnds, "\n")
      .replaceAll(rTabs, "    ")
      .replaceAll(rBlankLines, "")

  /**
   * All inline HTML blocks are hashified, so that no harm is done to their internals.
   */
  protected def hashHtmlBlocks(): this.type = {
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
      val endIdx = if (depth == 0) mTags.end else text.length
      val startIdx = m.start
      val html = text.substring(startIdx, endIdx)
      // Add to hash and apply replacement
      val key = randomKey
      htmlHash += key -> html
      val sb = new StringBuffer(text.subSequence(0, startIdx))
          .append(key)
          .append("\n\n")
          .append(text.subSequence(endIdx, text.length))
      text = sb
      // Continue recursively until all blocks are processes
      return hashHtmlBlocks
    } else return this
  }

  /**
   * Transforms the Markdown source into HTML.
   */
  def toHtml(): String = {
    normalize()
    hashHtmlBlocks()
    println(text)
    println(htmlHash)
    ""
  }

}
