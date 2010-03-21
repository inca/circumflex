package ru.circumflex.md

import java.util.regex._

/**
 * The processor for Markdown. Use `apply(String)` method for conversion.
 */
object Markdown {

  /* # Regex patterns */

  // * to standardize line endings
  private val rLineEnds = Pattern.compile("\\r\\n|\\r")
  // * to strip out whitespaces in blank lines
  private val rBlankLines = Pattern.compile("^[ \\t]+$", Pattern.MULTILINE)
  // * to replace tabs with spaces
  private val rTabs = Pattern.compile("(.*?)\\t")

  /* # The Processor */

  /**
   * Converts the `source` from Markdown to HTML.
   */
  def apply(source: String): String = {
    // standardize the input
    val text = new StringEx(source)
        .replaceAll(rLineEnds, "\n")
        .replaceAll(rTabs, "$1    ")
        .replaceAll(rBlankLines, "")
        .append("\n\n")
    

    return text.toString
  }

}

/* Utility stuff */

/**
 * Wraps the source into mutable StringBuilder with utility methods.
 */
class StringEx(source: CharSequence) {
  private var text = new StringBuffer(source)

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

  /**
   * Emits the wrapped content.
   */
  override def toString = text.toString
}

