package ru.circumflex.docco

import ru.circumflex.freemarker.DefaultConfiguration
import _root_.freemarker.template.Configuration
import java.io._

case class Section(private var _doc: String, private var _code: String) {
  private var _committed = false
  def this() = this("", "")
  def trimNewLines(s: String) = s.replaceAll("^\\n+(.*)", "$1").replaceAll("(.*?)\\n+$", "$1")
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
  def empty_?() = _doc == "" && _code == ""
}

/* # Scala source documentation utility */
class Docco(val file: File) {
  var sections: Seq[Section] = Nil
  /* Scala comment regex */
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
          sections ++= List(section)
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
  /* HTML exporting stuff */
  def toHtml(writer: Writer, template: String, ftlConfig: Configuration): Unit =
    ftlConfig.getTemplate(template)
        .process(Map[String, Any]("title" -> file.getName, "sections" -> sections), writer)
  def toHtml(writer: Writer, template: String): Unit = toHtml(writer, template, DefaultConfiguration)
  def toHtml(writer: Writer): Unit = toHtml(writer, "/default.html.ftl")
}

