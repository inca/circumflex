package circumflex
package freemarker

import core._
import java.io.File
import collection.JavaConversions
import org.apache.commons.io.FileUtils
import collection.mutable.HashMap

/*!# Static HTML generator

Circumflex FreeMarker Helper offers a tiny tool which scans `sourcePath` (relative
to `templatesRoot`) for templates (which are resolved from `templatesRoot`)
and generates corresponding HTML file into `targetDir` preserving directory
structure and names.

Any exceptions occured inside template will cause a template
to be omitted.

For example, consider following source directory
(`templatesRoot` = `src/main/resources/templates`, `sourcePath` = `/static`):

    src/main/resources/templates/static/
    \ errors/
    | \ 502.ftl
    | \ 404.ftl
    | \ layout.ftl
    \ layout.ftl
    \ index.ftl

After running `StaticHtmlGenerator` it will populate target directory with following
HTML files (assuming that `layout.ftl` cannot be rendered on their own):

    src/main/webapp/public/html/
    \ errors/
    | \ 502.html
    | \ 404.html
    \ index.ftl

You can also pass additional data using `add` method.
*/
class StaticHtmlGenerator(val templatesRoot: File,
                          val sourcePath: String,
                          val targetDir: File) {

  val sourceDir = new File(templatesRoot, sourcePath)
  val data = new HashMap[String, Any]

  def add(name: String, value: Any): this.type = {
    data += name -> value
    this
  }

  def generate() {
    val files = FileUtils.listFiles(sourceDir, Array("ftl"), true)
    FTL_LOG.info("Generating static html files into " + targetDir.getCanonicalPath)
    val t = time {
      JavaConversions.collectionAsScalaIterable[File](files).foreach { file =>
        val template = file.getCanonicalPath.substring(templatesRoot.getCanonicalPath.length())
        val relName = file.getCanonicalPath
            .substring(sourceDir.getCanonicalPath.length())
            .replaceAll("\\.ftl$", ".html")
        val outFile = new File(targetDir, relName)
        outFile.getParentFile.mkdirs()
        try {
          Context.executeInNew { ctx =>
            ctx.update("msg", msg)
            ctx.update("cfg", cx)
            ctx ++= data
            FileUtils.writeStringToFile(outFile, ftl2string(template), "UTF-8")
          }
        } catch {
          case e: Exception =>
            FTL_LOG.warn("Skipping " + template + " due to " + e.getClass.getSimpleName)
            FTL_LOG.debug("Exception in static generation", e)
        }
      }
    }
    FTL_LOG.info("Generated static html files in " + t._1.toDouble / 1000 + " seconds.")
  }

}