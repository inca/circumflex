package pro.savant.circumflex
package cluster

import java.io._
import org.apache.commons.io.FileUtils
import java.util.regex.{Matcher, Pattern}

class FileCopy(val srcDir: File,
               val dstDir: File) {

  if (!dstDir.exists)
    dstDir.mkdirs()

  def flat(f: File): Set[File] =
    if (f.isFile) Set(f)
    else f.listFiles().flatMap(flat _).toSet

  def flatPaths(f: File): Set[String] = {
    val basePath = f.getCanonicalPath
    flat(f).flatMap { f =>
      val p = f.getCanonicalPath
      if (p.startsWith(basePath))
        Some(p.substring(basePath.length).replaceAll("^/", ""))
      else None
    }
  }

  def copyNoOverwrite() {
    copyIf { (src, dst) => false }
  }

  def copyIfNewer() {
    copyIf { (src, dst) =>
      src.lastModified > dst.lastModified
    }
  }

  def copyOverwrite() {
    copyIf { (src, dst) => true }
  }

  protected def copyIf(predicate: (File, File) => Boolean) {
    val toCopy = flatPaths(srcDir).filter { p =>
      val src = new File(srcDir, p)
      val dst = new File(dstDir, p)
      predicate(src, dst)
    }
    currentMonitor.println("Copying " + toCopy.size + " files from " +
        srcDir + " to " + dstDir)
    toCopy.foreach { p =>
      val src = new File(srcDir, p)
      val dst = new File(dstDir, p)
      FileUtils.copyFile(src, dst, true)
    }
  }

  def filteredCopy(filenameRegex: Pattern,
                   props: Map[String, String]) {
    val toCopy = flatPaths(srcDir)
    currentMonitor.println("Copying " + toCopy.size + " files from " +
        srcDir + " to " + dstDir)
    toCopy.foreach { p =>
      val src = new File(srcDir, p)
      val dst = new File(dstDir, p)
      if (filenameRegex.matcher(src.getName).matches) {
        val in = new BufferedReader(new FileReader(src))
        val out = new FileWriter(dst)
        try {
          var line = in.readLine()
          while (line != null) {
            val sb = new StringBuffer
            val m = FILTER_TOKEN_PATTERN.matcher(line)
            while (m.find()) {
              val token = m.group(1)
              val replacement = props.get(token).getOrElse("${" + token + "}")
              m.appendReplacement(sb, Matcher.quoteReplacement(replacement))
            }
            m.appendTail(sb)
            out.write(sb.toString + "\n")
            line = in.readLine()
          }
        } finally {
          in.close()
          out.close()
        }
      } else FileUtils.copyFile(src, dst)
    }
  }

}