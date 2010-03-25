package ru.circumflex.md

import java.util.regex._

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

  /* ## Regex patterns */

  // standardize line endings
  val rLineEnds = Pattern.compile("\\r\\n|\\r")
  // strip out whitespaces in blank lines
  val rBlankLines = Pattern.compile("^ +$", Pattern.MULTILINE)
  // replace tabs with spaces
  val rTabs = Pattern.compile("\\t")

  /* ## Commons */



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

  /* ## Utility methods */

  /**
   * A convenient equivalent to `String.replaceAll` that accepts `Pattern`,
   * which is often compiled with `Pattern.MULTILINE`.
   */
  def replaceAll(pattern: Pattern, replacement: String): this.type = {
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
  def append(s: CharSequence): this.type = {
    text.append(s)
    return this
  }

  /**
   * Prepends the specified character sequence.
   */
  def prepend(s: CharSequence): this.type = {
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
  def normalize(): this.type =replaceAll(rLineEnds, "\n")
        .replaceAll(rTabs, "    ")
        .replaceAll(rBlankLines, "")

  def toHtml(): String = {

  }

}
