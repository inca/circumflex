package ru.circumflex
package docco

import core._, freemarker._
import java.io._
import org.apache.commons.io.filefilter.{TrueFileFilter, RegexFileFilter}
import java.util.{Collection => JCollection}
import collection.mutable.ListBuffer
import org.apache.commons.io.{FilenameUtils, IOUtils, FileUtils}

case class Section(private var _doc: String = "", private var _code: String = "") {

  var committed = false

  protected def trimNewLines(s: String) =
    s.replaceAll("^\\n+(.*)", "$1").replaceAll("(.*?)\\n+$", "$1")

  private var _md: String = null

  def addCode(s: String): this.type = {
    committed = true
    _code += s + "\n"
    this
  }
  def code = trimNewLines(_code)

  def addDoc(s: String): this.type = {
    _doc += s + "\n"
    _md = null
    this
  }
  def doc: String = {
    if (_md == null)
      _md = markeven.toHtml(_doc)
    _md
  }

  def isEmpty: Boolean = _doc == "" && _code == ""
}

/*!# Documenting Scala files with Docco

This utility generates a user-friendly HTML for specified Scala source file
by placing documentation and corresponding code blocks side by side.

The usage is trivial:

    Docco("my.scala").writeHtml("my.scala.html")

or shortcut of above:

    Docco("my.scala").writeHtml

Docco uses [FreeMarker][1] to process pages, so you can provide your own
FreeMarker `Configuration` and templates.

 [1]: http://freemarker.org "FreeMarker Templating Engine"
*/
object Docco {
  def apply(sourceFile: String, stripScaladoc: Boolean = true,
    useScaladoc: Boolean = false): Docco = 
  new Docco(new File(sourceFile), stripScaladoc, useScaladoc)
}

class Docco(val file: File, val stripScaladoc: Boolean = true,
    val useScaladoc: Boolean = false) {

  val pageTemplate: String = cx.get("docco.pageTemplate")
      .map(_.toString).getOrElse("/docco-single-page.html.ftl")
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
    def flushSection() {
      if (!section.isEmpty)
        result ++= List(section)
      section = new Section()
    }
    def flushSectionIfCommitted(s: String) {
      if (section.committed)
        flushSection()
        section.addDoc(s)
    }
    try {
      var str = reader.readLine
      while (str != null) {
        str match {
          case docSingleLine(s) if (!insideScaladoc) =>
            flushSectionIfCommitted(s)
            insideDoc = false
          case docBegin(i, s) if (!insideScaladoc) =>
            flushSectionIfCommitted(s)
            indent = i
            insideDoc = true
          case scaladocBegin(i, s) if (!insideDoc) =>
            if (useScaladoc) flushSectionIfCommitted(s)
            insideScaladoc = true
            if (!stripScaladoc) section.addCode(str)
          case docEnd(s) =>
            if (insideDoc)
              section.addDoc(s)
            if (insideScaladoc && useScaladoc)
              section.addDoc(s)
            if (insideScaladoc && !stripScaladoc)
              section.addCode(str)
            insideDoc = false
            insideScaladoc = false
          case s => {
            if (insideDoc) {
              section.addDoc(s.replaceAll("^" + indent, ""))
            } else if (insideScaladoc) {
              if (useScaladoc)
                section.addDoc(s.replaceAll("^" + indent, "").replaceFirst("\\*", ""))
            } else {
              if (!insideScaladoc || !stripScaladoc)
                section.addCode(s)
            }
          }
        }
        str = reader.readLine
      }
      flushSection()
    } finally {
      reader.close()
    }
    result
  }

  /* Export to HTML */
  def writeHtml(writer: Writer) {
    ftlConfig.getTemplate(pageTemplate)
        .process(Map[String, Any]("title" -> file.getName, "sections" -> sections), writer)
  }

  def writeHtml(file: File) {
    val fw = new FileWriter(file)
    try {
      writeHtml(fw)
    } finally {
      fw.close()
    }
  }

  def writeHtml(file: String) {
    writeHtml(new File(file))
  }

  def writeHtml() {
    writeHtml(file.getAbsolutePath + ".html")
  }

}

/*!## Batch processing

This utility generates Docco for specified `docco.basePath` configuration parameter.
It is intended to build a documentation suite for arbitrary Maven project.
The documentation is saved in location defined by `docco.outputDirectory` configuration
parameter and contains:

  * `index.html`;
  * generated subfolders and documentation;
  * custom resources in `.docco`.
*/
class DoccoBatch {
  // Base path for crawler
  val basePath: File = cx.get("docco.basePath") match {
    case Some(f: File) => f
    case Some(s: String) => new File(s)
    case _ => new File(".")
  }
  val outputPath = cx.get("docco.outputPath") match {
    case Some(f: File) => f
    case Some(s: String) => new File(s)
    case _ => new File("target/docco")
  }
  // Where should we store results of our work?
  // Templates
  val pageTemplate: String = cx.get("docco.pageTemplate")
      .map(_.toString).getOrElse("/docco-batch-page.html.ftl")
  val indexTemplate: String = cx.get("docco.indexTemplate")
      .map(_.toString).getOrElse("/docco-index.html.ftl")
  // Regex to filter sources
  val filenameRegex = cx.get("docco.filenameRegex").map(_.toString)
      .getOrElse(".*\\.scala$")
  // Custom resources
  var customResources: List[String] = Nil
  // Title for index
  val title: String = cx.get("docco.title").map(_.toString)
      .getOrElse(basePath.getCanonicalFile.getName + " index")
  // Should we strip Scaladoc ( `/** .. */` ) or not?
  val stripScaladoc: Boolean = cx.get("docco.stripScaladoc")
      .map(_.toString.toBoolean).getOrElse(true)
  // Should we ignore files with no docco?
  val skipEmpty: Boolean = cx.get("docco.skipEmpty")
      .map(_.toString.toBoolean).getOrElse(true)
  // Should we use Scaladoc for docco?
  val useScaladoc: Boolean = cx.get("docco.useScaladoc")
      .map(_.toString.toBoolean).getOrElse(false)

  def addCustomResource(v: String) { customResources ++= List(v) }

  def prepareCustomResources() {
    if (customResources.size > 0) {
      val customResDir = new File(outputPath, ".docco")
      // create output directories if they do not already exist
      FileUtils.forceMkdir(customResDir)
      // copy resources
      customResources.foreach {
        r =>
          var f = new File(r)
          if (f.isDirectory) FileUtils.copyDirectory(f, customResDir)
          else if (f.isFile) FileUtils.copyFile(f, customResDir)
          else {
            // try to load the resource as stream
            val res = getClass.getResource(r)
            if (res != null) {
              val in = res.openStream
              val out = new FileOutputStream(new File(customResDir, FilenameUtils.getName(r)))
              try {
                IOUtils.copy(in, out)
              } finally {
                in.close()
                out.close()
              }
            }
          }
      }
    }
  }

  def generate() {
    prepareCustomResources()
    // crawl basePath for the sources
    val bp = basePath.getCanonicalPath
    val op = outputPath.getCanonicalPath
    val sources = new ListBuffer[File]
    val srcIt = FileUtils.listFiles(basePath,
      new RegexFileFilter(filenameRegex),
      TrueFileFilter.INSTANCE).asInstanceOf[JCollection[File]].iterator
    while (srcIt.hasNext)
      sources += srcIt.next
    // generate doccos
    val doccos = sources.flatMap { f =>
      val docco = new Docco(f, stripScaladoc, useScaladoc)
      if (!skipEmpty || docco.sections.size > 1) {
        val fp = f.getCanonicalPath
        val relName = fp.substring(bp.length + 1) + ".html"
        val outFile = new File(outputPath, relName)
        FileUtils.forceMkdir(outFile.getParentFile)
        val out = new FileWriter(outFile)
        try {
          ftlConfig.getTemplate(pageTemplate).process(Map[String, Any](
            "title" -> f.getName,
            "sections" -> docco.sections,
            "depth" -> relName.toList.filter(c => c == File.separatorChar).length), out)
        } finally {
          out.close()
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
    val out = new FileWriter(new File(outputPath, "index.html"))
    try {
      var data = Map[String, Any]("dirs" -> dirs, "index" -> indexMap, "title" -> title)
      ftlConfig.getTemplate(indexTemplate).process(data, out)
    } finally {
      out.close()
    }
  }

}