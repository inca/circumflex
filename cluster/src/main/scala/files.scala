package pro.savant.circumflex
package cluster

import java.io.File
import org.apache.commons.io.FileUtils

class FileCopy(val srcDir: File,
               val dstDir: File) {

  if (!dstDir.exists)
    dstDir.mkdirs()

  def flat(f: File): Set[File] =
    if (f.isFile) Set(f)
    else f.listFiles().flatMap(flat _).toSet

  def flatPaths(f: File): Set[String] = {
    val basePath = f.getAbsolutePath
    flat(f).flatMap { f =>
      val p = f.getAbsolutePath
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
    CL_LOG.info("Copying " + toCopy.size + " files from " +
        srcDir + " to " + dstDir)
    toCopy.foreach { p =>
      val src = new File(srcDir, p)
      val dst = new File(dstDir, p)
      FileUtils.copyFile(src, dst, true)
    }
  }

}