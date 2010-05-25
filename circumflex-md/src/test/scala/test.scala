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
    "Inline HTML (Advanced)" in {
      "Inline HTML (Advanced)" must beFine
    }
    "Inline HTML (Simple)" in {
      "Inline HTML (Simple)" must beFine
    }
    "Inline HTML comments" in {
      "Inline HTML comments" must beFine
    }
    "Links, inline style" in {
      "Links, inline style" must beFine
    }
    "Links, reference style" in {
      "Links, reference style" must beFine
    }
    "Literal quotes in titles" in {
      "Literal quotes in titles" must beFine
    }
    "Nested blockquotes" in {
      "Nested blockquotes" must beFine
    }
    "Ordered and unordered lists" in {
      "Ordered and unordered lists" must beFine
    }
    "Strong and em together" in {
      "Strong and em together" must beFine
    }
    "Tabs" in {
      "Tabs" must beFine
    }
    "Tidyness" in {
      "Tidyness" must beFine
    }
    "SmartyPants" in {
      "SmartyPants" must beFine
    }
    "Markdown inside inline HTML" in {
      "Markdown inside inline HTML" must beFine
    }
    "Spans inside headers" in {
      "Spans inside headers" must beFine
    }
    "Macros" in {
      "Macros" must beFine
    }
  }
}
