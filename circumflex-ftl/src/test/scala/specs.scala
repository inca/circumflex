package ru.circumflex
package freemarker

import core._
import org.specs.runner.JUnit4
import org.specs.Specification
import org.apache.commons.io.FileUtils
import org.apache.commons.lang.StringUtils
import org.specs.matcher.Matcher
import java.io.File
import collection.mutable.HashMap

class SpecsTest extends JUnit4(CircumflexFtlSpec)

object CircumflexFtlSpec extends Specification {

  val beFine = new Matcher[String] {
    def apply(name: => String) = {
      val text = ftl2string("/" + name + ".ftl").trim
      val out = FileUtils.readFileToString(
        new File(this.getClass.getResource("/" + name + ".out").toURI), "UTF-8").trim
      val diffIndex = StringUtils.indexOfDifference(text, out)
      val diff = StringUtils.difference(text, out)
      (diffIndex == -1,
          "\"" + name + "\" is fine",
          "\"" + name + "\" fails at " + diffIndex + ": " + StringUtils.abbreviate(diff, 32))
    }
  }

  "Circumflex FTL" should {
    "handle simple objects" in {
      object simpleObject {
        val name = "Joe"
        val subobj = new Object {
          val name = "Smith"
        }
        override def toString = name + " " + subobj.name
      }
      'obj := simpleObject
      "Simple objects" must beFine
    }
    "handle lists" in {
      'list := List("one", "two", "three")
      'range := 0 to 9
      "Lists" must beFine
    }
    "handle maps" in {
      val map = new HashMap[String, Any]
      map += ("one" -> "Hello to")
      map += ("two" -> HashMap("one" -> "Joe", "two" -> "Smith"))
      'map := map
      "Maps" must beFine
    }
    "provide limited support of Scala XML" in {
      'root := <root>
        <child>
          <one id="0">1</one>
            <two id="1"/>
          <three id="2">Three</three>
        </child>
          <child/>
      </root>
      "XML" must beFine
    }
  }

}
