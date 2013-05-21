package circumflex
package xml

import java.io.File
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class CircumflexXmlSpec
    extends FreeSpec
    with MustMatchers {

  "StructHolder" - {
    "read contents of POM" in {
      val pom = new Pom(new File("pom.xml")).load()
      pom.groupId.getOrElse("") must equal("pro.savant.circumflex")
    }
  }

}

class Pom(val descriptorFile: File)
    extends StructHolder
    with XmlFile {

  def elemName = "project"

  val groupId = text("groupId")
  val artifactId = text("artifactId")
  val version = text("version")
  val packaging = text("packaging")

  override def toString = groupId.getOrElse("") + ":" +
      artifactId.getOrElse("") + ":" + version.getOrElse("")

}