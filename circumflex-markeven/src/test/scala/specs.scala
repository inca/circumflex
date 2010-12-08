package ru.circumflex.markeven

import org.specs.runner.JUnit4
import org.specs.Specification
import java.io.File
import org.apache.commons.io.FileUtils
import org.apache.commons.lang.StringUtils
import org.specs.matcher.Matcher

class SpecsTest extends JUnit4(MarkevenSpec)

object MarkevenSpec extends Specification {

  val beFine = new Matcher[String] {
    def apply(name: => String) = {
      val textFile = new File(this.getClass.getResource("/" + name + ".text").toURI)
      val htmlFile = new File(this.getClass.getResource("/" + name + ".html").toURI)
      val text = new MarkevenProcessor().toHtml(FileUtils.readFileToString(textFile, "UTF-8")).trim
      val html = FileUtils.readFileToString(htmlFile, "UTF-8")
          .trim
          .replaceAll("\\r\\n|\\r", "\n")
      val diffIndex = StringUtils.indexOfDifference(text, html)
      val diff = StringUtils.difference(text, html)
      (diffIndex == -1,
          "\"" + name + "\" is fine",
          "\"" + name + "\" fails at " + diffIndex + ": " + StringUtils.abbreviate(diff, 32))
    }
  }

  def process = addToSusVerb("process")

  val samples = "Amps and angle encoding" :: "Backslash escapes" ::
      "Blockquotes with code blocks" :: "Inline HTML" :: 
      "Links, inline style" :: "Links, reference style" :: "Literal quotes in titles" ::
      "Nested blockquotes" :: "Spans inside headers" :: "Em, strong, del" ::
      "Crazy test" :: Nil

  "MarkevenProcessor" should process {
    samples.map { s =>
      s in {
        s must beFine
      }
    }.apply(0)
  }
}
