package pro.savant.circumflex
package freemarker

import core._
import org.apache.commons.io.FileUtils
import org.apache.commons.lang.StringUtils
import java.io.File
import collection.mutable.HashMap
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import matchers.{MatchResult, MustMatchers, Matcher}

@RunWith(classOf[JUnitRunner])
class CircumflexFtlSpec
  extends FreeSpec
  with MustMatchers {


  val beFine = new Matcher[String] {
    def apply(name: String) = {
      val text = ftl2string("/" + name + ".ftl").trim
      val out = FileUtils.readFileToString(
        new File(this.getClass.getResource("/" + name + ".out").toURI), "UTF-8").trim
      val diffIndex = StringUtils.indexOfDifference(text, out)
      val diff = StringUtils.difference(text, out)
      MatchResult(diffIndex == -1,
          "\"" + name + "\" is fine",
          "\"" + name + "\" fails at " + diffIndex + ": " + StringUtils.abbreviate(diff, 32))
    }
  }

  "Circumflex FTL" - {
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
    "handle arrays" in {
      'arr := Array("one", "two", "three")
      "Arrays" must beFine
    }
  }

}
