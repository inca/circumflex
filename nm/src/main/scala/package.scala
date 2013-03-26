package pro.savant.circumflex

import core._
import java.io.File

package object nm {

  val LOG = new Logger("pro.savant.circumflex.nm")

  val root = new File(cx.getString("nm.root").getOrElse("target/.nm"))

  val domain = cx.getString("nm.domain").getOrElse("localhost")

}