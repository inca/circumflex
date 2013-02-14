package pro.savant.circumflex
package markeven

import pro.savant.circumflex._, core._
import java.io.{StringWriter, Writer}
import java.lang.StringBuilder
import java.util.regex.Pattern
import collection.mutable.HashSet
import java.util.Random

object const {
  val newLine = Pattern.compile("\r\n|\n|\r")
  val empty = Pattern.compile("\\s*")
  val entityRefefence = Pattern.compile("&(?:[a-zA-Z]+|(?:#[0-9]+|#[xX][0-9a-fA-F]+));")
  val htmlTag = Pattern.compile("</?([a-zA-Z]+)\\b.*?(/)?>", Pattern.DOTALL)
  val htmlComment = Pattern.compile("<!--.*?-->", Pattern.DOTALL)
  val backslashEscape = Pattern.compile("\\\\([\\.\\+\\*\\[\\]\\(\\)\\`\\{\\}\\_\\!\\-\\|\\~\\\\])")
  val fragment = Pattern.compile("\\{\\{([a-zA-Z0-9_-]+)\\}\\}")
  val fragmentBlock = Pattern.compile("\\{\\{\\{([a-zA-Z0-9_-]+)\\}\\}\\}\\s*")
  val inlineLink = Pattern.compile("\\((.*?)\\)")
  val refLink = Pattern.compile("\\[(.+?)\\]")
  val selector = Pattern.compile(
    "\\{(#[a-zA-Z0-9_-]+)?((\\.[a-zA-Z0-9_-]+)+)?\\}(?=[ \\t]*(?:\\n|\\r|\\Z))")
  val hr = Pattern.compile("---\\s*", Pattern.DOTALL)
  val table = Pattern.compile("-{3,}>?\\s+.+[\n|\r]\\s*-{3,}\\s*", Pattern.DOTALL)
  val tableSeparatorLine = Pattern.compile("[- :|]+(?=(?:\r\n|\n|\r)(?!\n|\r|\\Z))")
  val tableEndLine = Pattern.compile("\\s*-{3,}\\s*$")
  val ty_leftQuote = Pattern.compile("(?<=\\s|\\A|\\()(?:\"|&quot;)(?=\\S)")
  val ty_rightQuote = Pattern.compile("(?<=[\\p{L}\\d\\)\\]>?!.;:])(?:\"|&quot;)(?=[.,;:?!*\\)\\]<]|\\s|\\Z)")

  val blockTags = HashSet[String]("address", "article", "aside", "blockqoute", "canvas",
    "dd", "div", "dl", "dt", "fieldset", "figcaption", "figure", "footer", "form", "h1",
    "h2", "h3", "h4", "h5", "h6", "header", "hgroup", "hr", "nospript", "ol", "output",
    "p", "pre", "section", "table", "ul")
}

// Scrambler

class TextScrambler(val alphabet: String,
                    val percentage: Int) {

  def wrap(char: Char): String = "<span class=\"scr\">" + char + "</span>"

  val threshold: Double = {
    if (percentage < 0) 0d
    else if (percentage > 50) .5d
    else percentage / 100d
  }
  protected val rnd = new Random

  def getSpan: String = {
    if (rnd.nextDouble > threshold) ""
    else wrap(alphabet.charAt(rnd.nextInt(alphabet.size)))
  }
}

object EmptyTextScrambler extends TextScrambler("", 0) {
  override def getSpan = ""
}

// Processor

trait Processor {

  def out: Writer

  def renderer: MarkevenRenderer

  def process(cs: CharSequence) {
    val walk = cs match {
      case w: Walker => w
      case _ => new SubSeqWalker(cs)
    }
    run(walk)
  }

  def run(walk: Walker)

}

// Resolvables

class FragmentDef(val body: String,
                  val mode: ProcessingMode = ProcessingMode.NORMAL)

trait ProcessingMode

object ProcessingMode {
  object NORMAL extends ProcessingMode
  object CODE extends ProcessingMode
  object PLAIN extends ProcessingMode
}

class LinkDef(_url: String,
              _title: String = "") {

  val url = ampEscape.matcher(_url).replaceAll("&amp;")
  val title = wrapHtml(_title)

  def customAttrs: String = ""

  def writeLink(w: Writer, text: String) {
    w.write("<a href=\"")
    w.write(url)
    w.write("\"")
    if (title != "") {
      w.write(" title=\"")
      w.write(title)
      w.write("\"")
    }
    if (customAttrs != "") {
      w.write(" ")
      w.write(customAttrs)
    }
    w.write(">")
    w.write(text)
    w.write("</a>")
  }

  def toLink(text: String) = {
    val w = new StringWriter
    writeLink(w, text)
    w.toString
  }

  def writeMedia(w: Writer, alt: String) {
    w.write("<img src=\"")
    w.write(url)
    w.write("\"")
    if (title != "") {
      w.write(" title=\"")
      w.write(title)
      w.write("\"")
    }
    w.write(" alt=\"")
    w.write(alt)
    w.write("\"")
    if (customAttrs != "") {
      w.write(" ")
      w.write(customAttrs)
    }
    w.write("/>")
  }

  def toMedia(alt: String) = {
    val w = new StringWriter
    writeMedia(w, alt)
    w.toString
  }
}

// Block selector

class Selector(val renderer: MarkevenRenderer,
               var id: String = "",
               var classes: Seq[String] = Nil) {

  def nextIdCounter(): Int = {
    val result = ctx.getAs[Int]("markeven.processor.idCounter").getOrElse(0)
    ctx.update("markeven.processor.idCounter", result + 1)
    result
  }

  if (id == "" && renderer.autoAssignIdsPrefix != "") {
    id = renderer.autoAssignIdsPrefix + "-" + nextIdCounter()
  }

  def writeAttrs(w: Writer, idx: Int) {
    if (id != "") {
      w.write(" id=\"")
      w.write(id)
      w.write("\"")
    }
    if (classes.size > 0) {
      w.write(" class=\"")
      w.write(classes.mkString(" "))
      w.write("\"")
    }
    if (renderer.includeSourceIndex) {
      w.write(" data-source-index=\"")
      w.write(idx.toString)
      w.write("\"")
    }
  }

  override def toString = {
    val b = new StringBuilder("{")
    if (id != "")
      b.append("#").append(id)
    classes.foreach(c => b.append(".").append(c))
    b.append("}").toString
  }

}
