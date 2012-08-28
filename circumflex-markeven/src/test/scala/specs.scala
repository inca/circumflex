package ru.circumflex
package markeven

import org.specs.runner.JUnit4
import org.specs.Specification
import java.io.StringWriter

class SpecsTest extends JUnit4(MarkevenSpec)

object MarkevenSpec extends Specification {

  noDetailedDiffs()

  val pangram = "The quick brown fox jumps over the lazy dog."

  val testConf = new MarkevenConf {
    def resolveMedia(id: String) = id match {
      case "acorn" => Some(
        new LinkDef("http://eduarea.com/img/acorn.png", "EduArea Acorn Logo"))
      case _ => None
    }

    def resolveLink(id: String) = id match {
      case "ea" => Some(new LinkDef("http://eduarea.com", "EduArea & Friends"))
      case "cx" => Some(new LinkDef("http://circumflex.ru"))
      case _ => None
    }

    def resolveFragment(id: String) = id match {
      case "normal" => Some(new FragmentDef("normal *text* -- <em>process</em> it"))
      case "code" => Some(new FragmentDef("code, <em>a & b</em>", ProcessingMode.CODE))
      case "plain" => Some(new FragmentDef("plain, <em>a & b</em>", ProcessingMode.PLAIN))
      case _ => None
    }
  }

  "SubSeqWalker" should {
    "report range" in {
      val walker = new SubSeqWalker(pangram, -6, 63)
      walker.rangeStr must_== "[0,44)"
    }
    "deliver proper length, chars, and subsequences" in {
      val walker = new SubSeqWalker(pangram, 4, 19)
      walker.toString must_== "quick brown fox"
      walker.length must_== 15
      walker.charAt(6) must_== 'b'
      walker.subSequence(12, 15).toString must_== "fox"
    }
    "avoid reporting chars and sequences out of range" in {
      val walker = new SubSeqWalker(pangram, 4, 19)
      walker.subSequence(12, 16) must throwA[IndexOutOfBoundsException]
      walker.charAt(15)
      walker.charAt(16) must throwA[IndexOutOfBoundsException]
    }
    "work nested" in {
      val walker = new SubSeqWalker(pangram, 4, 19).subSequence(6, 11)
      walker.toString must_== "brown"
      walker.charAt(2) must_== 'o'
    }
    "iterate over characters" in {
      val walker = new SubSeqWalker(pangram, 4, 19)
      walker.hasNext must beTrue
      walker.current must_== 'q'
      walker.skip().current must_== 'u'
      walker.startFrom(6).current must_== 'b'
      walker.peek must_== 'r'
      walker.skip().current must_== 'r'
    }
    "perform basic lookups" in {
      val walker = new SubSeqWalker(pangram, 4, 19)
      walker.startFrom(6)
      walker.at("brown") must beTrue
      walker.current must_== 'b'
    }
    "recognize newlines" in {
      val walker = new SubSeqWalker("\r\n\n\r") // exactly 3 newlines
      walker.atNewLine must beTrue
      walker.skipNewLine().atNewLine must beTrue
      walker.skipNewLine().atNewLine must beTrue
      walker.hasNext must beFalse
      // now multiple lines
      walker.reset().skipNewLines().hasNext must beFalse
    }
    "recognize spaces" in {
      val walker = new SubSeqWalker(" \t ")
      walker.skipSpace().skipSpace().skipSpace().hasNext must beFalse
      walker.reset().skipSpaces().hasNext must beFalse
    }
    "recognize general whitespace" in {
      val walker = new SubSeqWalker(" \r\n\t\n")
      walker.skipWhitespace().skipWhitespace().at("\t") must beTrue
      walker.skipWhitespace().skipWhitespace().hasNext must beFalse
      walker.reset().skipWhitespaces().hasNext must beFalse
    }
    "recognize blank lines" in {
      val walker = new SubSeqWalker("  \n\n\t\n   Howdy!")
      walker.skipBlankLines().at("   Howdy!") must beTrue
    }
    "leave non-whitespace chars when skipping whitespace" in {
      val walker = new SubSeqWalker(pangram, 4, 19)
      walker.skipNewLine()
      walker.skipNewLines()
      walker.skipSpace()
      walker.skipSpaces()
      walker.skipWhitespace()
      walker.skipWhitespaces()
      walker.skipBlankLines()
      walker.at("quick") must beTrue
    }
    "process region exclusions" in {
      val w1 = new SubSeqWalker(pangram, 4, 19)
      val w2 = w1.exclude(6, 12)
      w2.toString must_== "quick fox"
    }
    "report space number correctly" in {
      val w = new SubSeqWalker("    * item\n")
      w.atSpaces(4) must beTrue
      w.atSpaces(5) must beFalse
    }
    "report absolute index" in {
      val walker = new SubSeqWalker(pangram, 4, 19)
      walker.getAbsoluteIndex(4) must_== 8
    }
  }

  "MultiSeqWalker" should {
    val w1 = new SubSeqWalker(pangram, 0, 10)
    val w2 = new SubSeqWalker(pangram, 16, 35)
    val w3 = new SubSeqWalker(pangram, 10, 16)
    val w4 = new SubSeqWalker(pangram, 40, 44)
    "deliver proper length, chars, and subsequences" in {
      val walker = new MultiSeqWalker(Seq(w1, w2, w3, w4))
      walker.toString must_== "The quick fox jumps over the brown dog."
      walker.length must_== 39
      walker.charAt(2) must_== 'e'
      walker.charAt(9) must_== ' '
      walker.charAt(10) must_== 'f'
      walker.charAt(31) must_== 'o'
      walker.subSequence(4, 13).toString must_== "quick fox"
      walker.subSequence(29, 38).toString must_== "brown dog"
      walker.subSequence(10, 10).toString must_== ""
      walker.subSequence(0, walker.length).toString must_== walker.toString
    }
    "nest correctly" in {
      val m1 = new MultiSeqWalker(Seq(w2, w3))
      val m2 = new MultiSeqWalker(Seq(w1, m1, w4))
      val walker = new SubSeqWalker(m2, 10, 34)
      walker.toString must_== "fox jumps over the brown"
      walker.length must_== 24
      walker.charAt(10) must_== 'o'
      walker.subSequence(15, 24).toString must_== "the brown"
      walker.subSequence(0, 0).toString must_== ""
      walker.subSequence(0, walker.length).toString must_== walker.toString
    }
    "process region exclusions" in {
      val m1 = new MultiSeqWalker(Seq(w1, w2, w3, w4))
      val m2 = m1.exclude(10, 35)
      m2.toString must_== "The quick dog."
    }
    "report absolute index" in {
      val walker = new MultiSeqWalker(Seq(w1, w2, w3, w4))
      walker.getAbsoluteIndex(29) must_== 10
      walker.getAbsoluteIndex(10) must_== 16
    }
  }

  "LinkDef" should {
    "render link and media" in {
      val ld = new LinkDef("http://eduarea.com", "EduArea Platform")
      ld.toLink("EduArea") must_== "<a href=\"http://eduarea.com\" " +
          "title=\"EduArea Platform\">EduArea</a>"
      ld.toMedia("") must_== "<img src=\"http://eduarea.com\" " +
          "title=\"EduArea Platform\" alt=\"\"/>"
    }
  }

  "Selector" should {
    "render attributes string" in {
      val sel = new Selector(testConf, "renaming", Seq("warning", "centered"))
      sel.toString must_== "{#renaming.warning.centered}"
    }
  }

  "Inline processor" should {
    "distinguish between ampersands and entity references" in {
      val input = "This & that; A&B -- but leave &amp;, &#095; and &#x09a; alone."
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "This &amp; that; A&amp;B &mdash; but leave &amp;, &#095; and &#x09a; alone."
    }
    "leave inline html tags ensuring correct amps" in {
      val input = "A link: <a href=\"/url?a=b&c=d\" " +
          "title=\"A&amp;B, *text* should stay.\">hello</a>"
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "A link: <a href=\"/url?a=b&amp;c=d\" " +
          "title=\"A&amp;B, *text* should stay.\">hello</a>"
    }
    "leave inline html comments unprocessed" in {
      val input = "This & <!-- this text is *unprocessed* --> that."
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "This &amp; <!-- this text is *unprocessed* --> that."
    }
    "escape &lt; and &gt; properly" in {
      val input = "A < B; B > A"
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "A &lt; B; B &gt; A"
    }
    "deal with bracing constructs like em and strong" in {
      var input = "Text *_ * _*"
      var w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "Text <strong>_ </strong> _*"
      input = "Some _* nested & not so easy *_ here"
      w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "Some <em><strong> nested &amp; not so easy </strong></em> here"
    }
    "respect backslash escapes" in {
      var input = "Text *_ \\* _*"
      var w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "Text <strong><em> * </em></strong>"
      input = "Some \\_\\* nested & not so easy *_ here"
      w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "Some _* nested &amp; not so easy *_ here"
    }
    "process triple code spans" in {
      val input = "The ```<em>code</em> & _almost_ untouched\\.```."
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "The <code><em>code</em> &amp; _almost_ untouched\\.</code>."
    }
    "process regular code spans" in {
      val input = "Nested `<html> tags & SGMLs &#095;` are cool."
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "Nested <code>&lt;html&gt; tags &amp; SGMLs &#095;</code> are cool."
    }
    "respect MathJax formulas" in {
      val input = "%% A < B %% and $$ a<c>d $$"
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "%% A &lt; B %% and $$ a&lt;c&gt;d $$"
    }
    "resolve fragments" in {
      var input = "In text: {{normal}}\nIn code: `{{normal}}`"
      var w = new StringWriter
      new InlineProcessor(w, testConf).process(input)
      w.toString must_== "In text: normal " +
          "<strong>text</strong> &mdash; <em>process</em> it\n" +
          "In code: <code>normal <strong>text</strong> &mdash; <em>process</em> it</code>"
      input = "In text: {{code}}\nIn code: `{{code}}`"
      w = new StringWriter
      new InlineProcessor(w, testConf).process(input)
      w.toString must_== "In text: code, &lt;em&gt;a &amp; b&lt;/em&gt;\n" +
          "In code: <code>code, &lt;em&gt;a &amp; b&lt;/em&gt;</code>"
      input = "In text: {{plain}}\nIn code: `{{plain}}`"
      w = new StringWriter
      new InlineProcessor(w, testConf).process(input)
      w.toString must_== "In text: plain, <em>a &amp; b</em>\n" +
          "In code: <code>plain, <em>a &amp; b</em></code>"
    }
    "process inline links" in {
      var input = "Simple [_inline_ link](/url)."
      var w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "Simple <a href=\"/url\"><em>inline</em> link</a>."
      input = "Kinda \\[[confusing [\\]() now](/?a=b&c=d)]"
      w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "Kinda [<a href=\"/?a=b&amp;c=d\">confusing []() now</a>]"
    }
    "process inline images" in {
      val input = "Acorn: ![*not* processed & escaped](/acorn.png)"
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "Acorn: <img src=\"/acorn.png\" alt=\"*not* processed &amp; escaped\"/>"
    }
    "process reference links and images" in {
      val input = "[A & B][ea]\n[Lookup `<a>` in book][cx]\n![][acorn]"
      val w = new StringWriter
      new InlineProcessor(w, testConf).process(input)
      w.toString must_== "<a href=\"http://eduarea.com\" " +
          "title=\"EduArea &amp; Friends\">A &amp; B</a>\n" +
          "<a href=\"http://circumflex.ru\">Lookup <code>&lt;a&gt;</code> in book</a>\n" +
          "<img src=\"http://eduarea.com/img/acorn.png\" title=\"EduArea Acorn Logo\" alt=\"\"/>"
    }
    "process mixed inline-reference links" in {
      val input = "[A & B](ea)\n[Lookup `<a>` in book][cx]\n[](acorn)"
      val w = new StringWriter
      new InlineProcessor(w, testConf).process(input)
      w.toString must_== "<a href=\"ea\">A &amp; B</a>\n" +
          "<a href=\"http://circumflex.ru\">Lookup <code>&lt;a&gt;</code> in book</a>\n" +
          "<a href=\"acorn\"></a>"
    }
    "process headless links" in {
      val input = "Headless link to [[ea]]"
      val w = new StringWriter
      new InlineProcessor(w, testConf).process(input)
      w.toString must_== "Headless link to " +
          "<a href=\"http://eduarea.com\" title=\"EduArea &amp; Friends\">" +
          "EduArea &amp; Friends</a>"
    }
    "process basic typographics" in {
      val input = "\"This & that\" -- &quot;(c) (r) (TM)&quot; ..."
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must_== "&laquo;This &amp; that&raquo; &mdash; &laquo;&copy; &reg; &trade;&raquo; &hellip;"
    }
  }

}