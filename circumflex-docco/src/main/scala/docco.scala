package ru.circumflex.docco

import ru.circumflex.core._
import ru.circumflex.freemarker._
import java.io._
import ru.circumflex.md.Markdown
import org.apache.commons.io.filefilter.{TrueFileFilter, RegexFileFilter}
import java.util.{Collection => JCollection}
import collection.mutable.ListBuffer
import org.apache.commons.io.{FilenameUtils, IOUtils, FileUtils}
import collection.immutable.ListMap

/**
 * A simple wrapper over a Documentation -> Code Block tuple.
 */
case class Section(private var _doc: String = "", private var _code: String = "") {

  private var _committed = false
  def committed_?() = _committed

  protected def trimNewLines(s: String) =
    s.replaceAll("^\\n+(.*)", "$1").replaceAll("(.*?)\\n+$", "$1")

  private var _md: String = null

  def addCode(s: String): this.type = {
    _committed = true
    _code += s + "\n"
    return this
  }
  def code = trimNewLines(_code)

  def addDoc(s: String): this.type = {
    _doc += s + "\n"
    _md = null
    return this
  }
  def doc: String = {
    if (_md == null)
      _md = Markdown(_doc)
    return _md
  }

  def empty_?() = _doc == "" && _code == ""
}

/*!# Documenting Scala files with Docco

This utility generates a user-friendly HTML for specified Scala source file
by placing documentation and corresponding code blocks side by side.

The usage is trivial:

    Docco("my.scala").toHtml("my.scala.html")

or shortcut of above:

    Docco("my.scala").toHtml

Docco uses [FreeMarker][1] to process pages, so you can provide your own
FreeMarker `Configuration` and templates.

 [1]: http://freemarker.org "FreeMarker Templating Engine"
*/

object Docco {
  val DEFAULT_SINGLE_PAGE_TEMPLATE = "/docco-single-page.html.ftl"
  val DEFAULT_BATCH_PAGE_TEMPLATE = "/docco-batch-page.html.ftl"
  val DEFAULT_INDEX_TEMPLATE = "/docco-index.html.ftl"
  def apply(sourceFile: String, stripScaladoc: Boolean = true): Docco =
    new Docco(new File(sourceFile), stripScaladoc)
}

class Docco(val file: File, val stripScaladoc: Boolean = true) {
  import Docco._
  var pageTemplate: String = DEFAULT_SINGLE_PAGE_TEMPLATE

  val docSingleLine = "^\\s*/\\*!\\s*(.*?)\\*/".r
  val docBegin = "^(\\s*)/\\*!\\s*(.*)".r
  val docEnd = "(.*?)\\*/\\s*".r
  val scaladocBegin = "^(\\s*)/\\*\\*(.*)".r

  val sections = {
    var result = new ListBuffer[Section]
    var section = new Section()
    val reader = new BufferedReader(new FileReader(file))
    var insideDoc = false
    var insideScaladoc = false
    var indent = ""
    def flushSection(): Unit = {
      if (!section.empty_?)
        result ++= List(section)
      section = new Section()
    }
    try {
      var str = reader.readLine
      while (str != null) {
        str match {
          case docSingleLine(s) if (!insideScaladoc) =>
            if (section.committed_?)
              flushSection
            section.addDoc(s)
            insideDoc = false
          case docBegin(i, s) if (!insideScaladoc) =>
            if (section.committed_?)
              flushSection
            section.addDoc(s)
            indent = i
            insideDoc = true
          case scaladocBegin(i, s) if (!insideDoc) =>
            insideScaladoc = true
            if (!stripScaladoc) section.addCode(str)
          case docEnd(s) =>
            if (insideDoc)
              section.addDoc(s)
            if (insideScaladoc && !stripScaladoc)
              section.addCode(str)
            insideDoc = false
            insideScaladoc = false
          case s => {
            if (insideDoc) {
              section.addDoc(s.replaceAll("^" + indent, ""))
            } else {
              if (!insideScaladoc || !stripScaladoc)
                section.addCode(s)
            }
          }
        }
        str = reader.readLine
      }
      flushSection
    } finally {
      reader.close()
    }
    result
  }

  /* Export to HTML */

  def toHtml(writer: Writer): Unit = ftlConfig.getTemplate(pageTemplate)
      .process(Map[String, Any]("title" -> file.getName, "sections" -> sections), writer)

  def toHtml(file: File): Unit = {
    val fw = new FileWriter(file)
    try {
      toHtml(fw)
    } finally {
      fw.close
    }
  }

  def toHtml(file: String): Unit = toHtml(new File(file))

  def toHtml(): Unit = toHtml(file.getAbsolutePath + ".html")

}

/*!## Batch processing

This utility generates Docco for specified `basePath`. It is intended to
build a documentation suite for arbitrary Maven project. The documentation
is saved in `outputDirectory` and contains:

  * `index.html`
  * folders and subfolders with generated Docco
  * custom resources in `.docco`
*/
class DoccoBatch(val basePath: File, val outputDirectory: File) {
  import Docco._
  // FreeMarker stuff
  var pageTemplate: String = DEFAULT_BATCH_PAGE_TEMPLATE
  var indexTemplate: String = DEFAULT_INDEX_TEMPLATE
  // Regex to filter sources
  var filenameRegex = ".*\\.scala$"
  // Custom resources
  var customResources: List[String] = Nil
  // Title for index
  var title: String = basePath.getCanonicalFile.getName + " index"
  // Should we strip Scaladoc ( /** ... */ ) or not?
  var stripScaladoc: Boolean = true
  // Should we ignore files with no docco?
  var skipEmpty: Boolean = true

  // For interop with Java
  def setPageTemplate(v: String) = { pageTemplate = v }
  def setIndexTemplate(v: String) = { indexTemplate = v }
  def setFilenameRegex(v: String) = { filenameRegex = v }
  def setStripScaladoc(v: Boolean) = { stripScaladoc = v }
  def setSkipEmpty(v: Boolean) = { skipEmpty = v }
  def setTitle(v: String) = { title = v }
  def addCustomResource(v: String) = { customResources ++= List(v) }

  /**
   * Use this method to build the documentation suite.
   */
  def generate(): Unit = {
    // prepare custom resources
    if (customResources.size > 0) {
      val customResDir = new File(outputDirectory, ".docco")
      // create output directories if they do not already exist
      FileUtils.forceMkdir(customResDir)
      // copy resources
      for (r <- customResources) {
        var f = new File(r)
        if (f.isDirectory) FileUtils.copyDirectory(f, customResDir)
        else if (f.isFile) FileUtils.copyFile(f, customResDir)
        else {    // try to load the resource as stream
          val res = getClass.getResource(r)
          if (res != null) {
            val in = res.openStream
            val out = new FileOutputStream(new File(customResDir, FilenameUtils.getName(r)))
            try {
              IOUtils.copy(in, out)
            } finally {
              in.close
              out.close
            }
          }
        }
      }
    }
    // crawl basePath for the sources
    val bp = basePath.getCanonicalPath
    val op = outputDirectory.getCanonicalPath
    val sources = new ListBuffer[File]
    val srcIt = FileUtils.listFiles(basePath,
      new RegexFileFilter(filenameRegex),
      TrueFileFilter.INSTANCE).asInstanceOf[JCollection[File]].iterator
    while (srcIt.hasNext)
      sources += srcIt.next
    // generate doccos
    val doccos = sources.flatMap { f =>
      val docco = new Docco(f, stripScaladoc)
      if (!skipEmpty || docco.sections.size > 1) {
        val fp = f.getCanonicalPath
        val relName = fp.substring(bp.length + 1) + ".html"
        val outFile = new File(outputDirectory, relName)
        FileUtils.forceMkdir(outFile.getParentFile)
        val out = new FileWriter(outFile)
        try {
          var data = Map[String, Any](
            "title" -> f.getName,
            "sections" -> docco.sections,
            "depth" -> relName.toList.filter(c => c == File.separatorChar).length)
          ftlConfig.getTemplate(pageTemplate).process(data, out)
        } finally {
          out.close
        }
        Some(outFile)
      } else None
    }
    // prepare index
    val indexMap = doccos
        .groupBy(f => f.getParentFile).map { p =>
      val dirName = p._1.getCanonicalPath.substring(op.length + 1)
      val filenames = p._2.map(f => f.getName.replaceAll("\\.html$", ""))
      (dirName -> filenames)
    }
    val dirs = indexMap.keys.toList.sortBy(_.toString)
    val out = new FileWriter(new File(outputDirectory, "index.html"))
    try {
      var data = Map[String, Any]("dirs" -> dirs, "index" -> indexMap, "title" -> title)
      ftlConfig.getTemplate(indexTemplate).process(data, out)
    } finally {
      out.close
    }
  }

}