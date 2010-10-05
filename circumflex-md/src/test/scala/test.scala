package ru.circumflex.md.test

import org.specs.runner.JUnit4
import org.specs.Specification
import java.io.File
import ru.circumflex.md.Markdown
import org.apache.commons.io.FileUtils
import org.apache.commons.lang.StringUtils
import org.specs.matcher.Matcher

class SpecsTest extends JUnit4(MarkdownSpec)

object MarkdownSpec extends Specification {

  val beFine = new Matcher[String] {
    def apply(name: => String) = {
      val textFile = new File(this.getClass.getResource("/" + name + ".text").toURI)
      val htmlFile = new File(this.getClass.getResource("/" + name + ".html").toURI)
      val text = Markdown(FileUtils.readFileToString(textFile, "UTF-8")).trim
      val html = FileUtils.readFileToString(htmlFile, "UTF-8").trim
      val diffIndex = StringUtils.indexOfDifference(text, html)
      val diff = StringUtils.difference(text, html)
      (diffIndex == -1,
          "\"" + name + "\" is fine",
          "\"" + name + "\" fails at " + diffIndex + ": " + StringUtils.abbreviate(diff, 32))
    }
  }

  def process = addToSusVerb("process")

  val samples = "Amps and angle encoding" :: "Auto links" :: "Backslash escapes" ::
      "Blockquotes with code blocks" :: "Hard-wrapped paragraphs with list-like lines" ::
      "Horizontal rules" :: "Inline HTML (Advanced)" :: "Inline HTML (Simple)" ::
      "Inline HTML comments" :: "Links, inline style" :: "Links, reference style" ::
      "Literal quotes in titles" :: "Nested blockquotes" :: "Ordered and unordered lists" ::
      "Strong and em together" :: "Tabs" :: "Tidyness" :: "SmartyPants" ::
      "Markdown inside inline HTML" :: "Spans inside headers" :: "Macros" :: Nil

  "MarkdownProcessor" should process {
    samples.map { s =>
      s in {
        s must beFine
      }
    }.apply(0)
  }
}
