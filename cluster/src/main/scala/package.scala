package pro.savant.circumflex

import core._
import java.io.File

package object cluster {

  val LOG = new Logger("pro.savant.circumflex.cluster")

  val root = cx.getString("cx.cluster.root") match {
    case Some(path) => new File(path)
    case _ => new File(System.getProperty("user.home"), ".cx")
  }

  val domain = cx.getString("cx.cluster.domain").getOrElse("localhost")

}