package ru.circumflex.md.test

import org.specs.runner.JUnit4
import org.specs.Specification
import java.io.File
import ru.circumflex.md.Markdown
import org.apache.commons.io.{IOUtils, FileUtils}
import org.specs.matcher.Matcher

class SpecsTest extends JUnit4(MarkdownSpec)

object MarkdownSpec extends Specification {

  val beOK = new Matcher[String] {
    def apply(name: => String) = {
      val textFile = new File(this.getClass.getResource("/" + name + ".text").toURI)
      val htmlFile = new File(this.getClass.getResource("/" + name + ".html").toURI)
      val text = FileUtils.readFileToString(textFile, "UTF-8")
      val html = FileUtils.readFileToString(htmlFile, "UTF-8")
      (Markdown(text).trim == html.trim, "\"" + name + "\" is fine", "\"" + name + "\" fails")
    }
  }

  "MarkdownProcessor" should {
    "correctly encode ampersands and angles" in {
        "Amps and angle encoding" must beOK
    }
  }
}