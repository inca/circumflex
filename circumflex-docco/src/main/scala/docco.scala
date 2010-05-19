package ru.circumflex.docco

import ru.circumflex.freemarker.DefaultConfiguration
import _root_.freemarker.template.Configuration
import java.io._
import ru.circumflex.md.Markdown
import org.slf4j.LoggerFactory
import org.apache.commons.io.filefilter.{TrueFileFilter, RegexFileFilter}
import java.util.{Comparator, Collections, Collection => JCollection}
import scala.collection.{Map => SMap}
import ru.circumflex.core.CircumflexUtil
import collection.mutable.ListBuffer
import org.apache.commons.io.{FilenameUtils, IOUtils, FileUtils}

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

/**
 * ## Processing single files
 *
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
 * Docco uses [FreeMarker][1] to process pages, so you can provide your own
 * FreeMarker `Configuration` and templates.
 *
 * [1]: http://freemarker.org "FreeMarker Templating Engine"
 *
 */

object Docco {
  val log = LoggerFactory.getLogger("ru.circumflex.docco")
  val DEFAULT_SINGLE_PAGE_TEMPLATE = "/single-page.html.ftl"
  val DEFAULT_BATCH_PAGE_TEMPLATE = "/batch-page.html.ftl"
  val DEFAULT_INDEX_TEMPLATE = "/index.html.ftl"
  def apply(sourceFile: File): Docco = new Docco(sourceFile)
  def apply(sourceFile: String): Docco = apply(new File(sourceFile))
}

class Docco(val file: File) {
  import Docco._
  /* FreeMarker stuff */
  var ftlConfig: Configuration = DefaultConfiguration
  var pageTemplate: String = DEFAULT_SINGLE_PAGE_TEMPLATE

  /* Scala comments regex */
  val commentBegin = "^\\s*/\\*\\*? ?(.*)".r
  val commentEnd = "^(.*?)\\*/\\s*".r
  val commentSingleLine = "^\\s*// ?(.*)".r
  val commentSingleBlock = "^\\s*/\\* ?(.*?)\\*/\\s*".r
  val commentBody = "^(?:\\s*\\*)? ?(.*)".r

  /* Parse sections */
  var sections: Seq[Section] = Nil
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

/**
 * ## Batch processing
 *
 * This utility generates Docco for specified `basePath`. It is intended to
 * build a documentation suite for arbitrary Maven project. The documentation
 * is saved in `outputDirectory` and contains:
 *
 *  * `index.html`
 *  * folders and subfolders with generated Docco
 *  * custom resources in `.docco`
 */
class DoccoBatch(val basePath: File, val outputDirectory: File) {
  import Docco._
  /* FreeMarker stuff */
  var ftlConfig: Configuration = DefaultConfiguration
  var pageTemplate: String = DEFAULT_BATCH_PAGE_TEMPLATE
  var indexTemplate: String = DEFAULT_INDEX_TEMPLATE
  /* Regex to filter sources */
  var filenameRegex = ".*\\.scala$"
  /* Custom resources */
  var customResources: List[String] = Nil
  /* Title for index */
  var title: String = basePath.getCanonicalFile.getName + " index"
  /* Filename comparator */
  val fileComparator = new Comparator[File] {
    def compare(f1: File, f2: File) = f1.getName.compareTo(f2.getName)
  }

  /* For interop with Java */
  def setPageTemplate(v: String) = { pageTemplate = v }
  def setIndexTemplate(v: String) = { indexTemplate = v }
  def setFilenameRegex(v: String) = { filenameRegex = v }
  def setTitle(v: String) = { title = v }
  def addCustomResource(v: String) = { customResources ++= List(v) }

  /* Sources map */
  var sourcesMap: SMap[File, Seq[File]] = _
  var indexMap: Map[String, Seq[String]] = _

  /**
   * Use this method to build the documentation suite.
   */
  def generate(): Unit = {
    // prepare custom resources
    customResources ++= List("/docco/main.css")
    customResources ++= List("/docco/highlight.js")
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
        } else log.warn("Skipping non-existent resource: " + r)
      }
    }
    // crawl basePath for the sources
    val bp = basePath.getCanonicalPath
    val sources = new ListBuffer[File]
    val srcIt = FileUtils.listFiles(basePath,
      new RegexFileFilter(filenameRegex),
      TrueFileFilter.INSTANCE).asInstanceOf[JCollection[File]].iterator
    while (srcIt.hasNext)
      sources += srcIt.next
    // generate doccos
    val doccos = sources.map(f => {
      val fp = f.getCanonicalPath
      val relName = fp.substring(bp.length + 1) + ".html"
      val outFile = new File(outputDirectory, relName)
      FileUtils.forceMkdir(outFile.getParentFile)
      val docco = Docco(f)
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
      outFile
    })
    // prepare index
    sourcesMap = CircumflexUtil.groupBy[File, File](sources, f => f.getParentFile)
    indexMap = Map[String, Seq[String]]()
    sourcesMap.foreach(p => {
      val dirName = p._1.getCanonicalPath.substring(bp.length + 1)
      val filenames = p._2.map(f => f.getName)
      indexMap += dirName -> filenames
    })
    val out = new FileWriter(new File(outputDirectory, "index.html"))
    try {
      var data = Map[String, Any]("index" -> indexMap, "title" -> title)
      ftlConfig.getTemplate(indexTemplate).process(data, out)
    } finally {
      out.close
    }
  }

}
