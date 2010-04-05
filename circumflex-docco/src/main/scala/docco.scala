package ru.circumflex.docco

import ru.circumflex.freemarker.DefaultConfiguration
import _root_.freemarker.template.Configuration
import java.io._
import ru.circumflex.md.Markdown

/**
 * A simple wrapper over a Documentation -> Code Block tuple.
 */
case class Section(private var _doc: String, private var _code: String) {
  private var _committed = false
  def this() = this("", "")
  def trimNewLines(s: String) =
    s.replaceAll("^\\n+(.*)", "$1")
        .replaceAll("(.*?)\\n+$", "$1")
  def committed_?() = _committed
  def code = trimNewLines(_code)
  def doc = trimNewLines(_doc)
  def addCode(s: String): this.type = {
    _committed = true
    _code += s + "\n"
    return this
  }
  def addDoc(s: String): this.type = {
    _doc += s + "\n"
    return this
  }
  def md(): this.type = {
    _doc = Markdown(_doc)
    return this
  }
  def empty_?() = _doc == "" && _code == ""
}

/* # Scala source documentation utility */

/**
 * This utility generates a user-friendly HTML for specified Scala source file
 * by placing documentation and corresponding code blocks side by side.
 *
 * The usage is trivial:
 *
 *     Docco("my.scala").toHtml("my.scala.html")
 *
 * or shortcut of above:
 *
 *     Docco("my.scala").toHtml
 *
 * or with custom template:
 *
 *     Docco("my.scala").toHtml("my.scala.html", "/my-custom-template.html.ftl")
 *
 * or even with custom freemarker configuration:
 *
 *     Docco("my.scala").toHtml("my.scala.html", "/my-custom-template.html.ftl", myFtlCfg)
 *
 */

object Docco {
  val DEFAULT_SINGLE_PAGE_TEMPLATE = "/single-page.html.ftl"
  val DEFAULT_BATCH_PAGE_TEMPLATE = "/batch-page.html.ftl"
  val DEFAULT_INDEX_TEMPLATE = "/index.html.ftl"
  def apply(sourceFile: File): Docco = new Docco(sourceFile)
  def apply(sourceFile: String): Docco = apply(new File(sourceFile))
}

class Docco(val file: File) {
  var sections: Seq[Section] = Nil
  /* Scala comments regex */
  val commentBegin = "^\\s*/\\*\\*? ?(.*)".r
  val commentEnd = "^(.*?)\\*/\\s*".r
  val commentSingleLine = "^\\s*// ?(.*)".r
  val commentSingleBlock = "^\\s*/\\* ?(.*?)\\*/\\s*".r
  val commentBody = "^(?:\\s*\\*)? ?(.*)".r
  /* Parse Scala comments */
  parse()
  def parse() = {
    val reader = new BufferedReader(new FileReader(file))
    try {
      var section: Section = new Section()
      var insideComment = false
      var str = reader.readLine
      def flushSection() = {
        if (!section.empty_?)
          sections ++= List(section.md)
        section = new Section()
      }
      while (str != null) {
        str match {
          case commentSingleLine(s) =>
            if (section.committed_?)
              flushSection
            section.addDoc(s)
          case commentSingleBlock(s) =>
            if (section.committed_?)
              flushSection
            section.addDoc(s)
            insideComment = false
          case commentBegin(s) =>
            if (section.committed_?)
              flushSection
            insideComment = true
            section.addDoc(s)
          case commentEnd(s) if insideComment =>
            insideComment = false
            section.addDoc(s)
          case commentBody(s) if insideComment =>
            section.addDoc(s)
          case s => section.addCode(s)
        }
        str = reader.readLine
      }
      flushSection
    } finally {
      reader.close()
    }
  }

  /* ## Exporting stuff */

  /* For writers */
  def toHtml(writer: Writer, template: String, ftlConfig: Configuration): Unit =
    ftlConfig.getTemplate(template)
        .process(Map[String, Any]("title" -> file.getName, "sections" -> sections), writer)
  def toHtml(writer: Writer, template: String): Unit =
    toHtml(writer, template, DefaultConfiguration)
  def toHtml(writer: Writer): Unit = toHtml(writer, Docco.DEFAULT_SINGLE_PAGE_TEMPLATE)
  /* Now for files */
  def toHtml(file: File, template: String, ftlConfig: Configuration): Unit = {
    val fw = new FileWriter(file)
    try {
      toHtml(fw, template, ftlConfig)
    } finally {
      fw.close
    }
  }
  def toHtml(file: File, template: String): Unit =
    toHtml(file, template, DefaultConfiguration)
  def toHtml(file: File): Unit =
    toHtml(file, Docco.DEFAULT_SINGLE_PAGE_TEMPLATE)
  /* Now for strings with filepaths */
  def toHtml(file: String, template: String, ftlConfig: Configuration): Unit =
    toHtml(new File(file), template, ftlConfig)
  def toHtml(file: String, template: String): Unit =
    toHtml(file, template, DefaultConfiguration)
  def toHtml(file: String): Unit =
    toHtml(file, Docco.DEFAULT_SINGLE_PAGE_TEMPLATE)
  /* And now kinda "look, ma, no hands" */
  def toHtml(): Unit = toHtml(file.getAbsolutePath + ".html")
}

