package ru.circumflex.md.test

import org.specs.runner.JUnit4
import org.specs.Specification
import java.io.File
import ru.circumflex.md.Markdown
import org.apache.commons.io.{IOUtils, FileUtils}
import org.specs.matcher.Matcher
import org.specs.specification.Example

class SpecsTest extends JUnit4(MarkdownSpec)

object MarkdownSpec extends Specification {

  val beFine = new Matcher[String] {
    def apply(name: => String) = {
      val textFile = new File(this.getClass.getResource("/" + name + ".text").toURI)
      val htmlFile = new File(this.getClass.getResource("/" + name + ".html").toURI)
      val text = FileUtils.readFileToString(textFile, "UTF-8")
      val html = FileUtils.readFileToString(htmlFile, "UTF-8")
      (Markdown(text).trim == html.trim, "\"" + name + "\" is fine", "\"" + name + "\" fails")
    }
  }

  def process = addToSusVerb("process")

  "MarkdownProcessor" should process {
    "Amps and angle encoding" in {
      "Amps and angle encoding" must beFine
    }
    "Auto links" in {
      "Auto links" must beFine
    }
    "Backslash escapes" in {
      "Backslash escapes" must beFine
    }
    "Blockquotes with code blocks" in {
      "Blockquotes with code blocks" must beFine
    }
    "Hard-wrapped paragraphs with list-like lines" in {
      "Hard-wrapped paragraphs with list-like lines" must beFine
    }
    "Horizontal rules" in {
      "Horizontal rules" must beFine
    }
  }
}