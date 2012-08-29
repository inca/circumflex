package ru.circumflex
package markeven

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers
import java.io.StringWriter


@RunWith(classOf[JUnitRunner])
class MarkevenSpec
  extends FreeSpec
  with MustMatchers {

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

  "SubSeqWalker" - {
    "report range" in {
      val walker = new SubSeqWalker(pangram, -6, 63)
      walker.rangeStr must equal ("[0,44)")
    }
    "deliver proper length, chars, and subsequences" in {
      val walker = new SubSeqWalker(pangram, 4, 19)
      walker.toString must equal ("quick brown fox")
      walker.length must equal (15)
      walker.charAt(6) must equal ('b')
      walker.subSequence(12, 15).toString must equal ("fox")
    }
    "avoid reporting chars and sequences out of range" in {
      val walker = new SubSeqWalker(pangram, 4, 19)
      evaluating  {walker.subSequence(12, 16)} must produce [IndexOutOfBoundsException]
      walker.charAt(15)
      evaluating  {walker.charAt(16)} must produce [IndexOutOfBoundsException]
    }
    "work nested" in {
      val walker = new SubSeqWalker(pangram, 4, 19).subSequence(6, 11)
      walker.toString must equal ("brown")
      walker.charAt(2) must equal ('o')
    }
    "iterate over characters" in {
      val walker = new SubSeqWalker(pangram, 4, 19)
      walker.hasNext must equal (true)
      walker.current must equal ('q')
      walker.skip().current must equal ('u')
      walker.startFrom(6).current must equal ('b')
      walker.peek must equal ('r')
      walker.skip().current must equal ('r')
    }
    "perform basic lookups" in {
      val walker = new SubSeqWalker(pangram, 4, 19)
      walker.startFrom(6)
      walker.at("brown") must equal (true)
      walker.current must equal ('b')
    }
    "recognize newlines" in {
      val walker = new SubSeqWalker("\r\n\n\r") // exactly 3 newlines
      walker.atNewLine must equal (true)
      walker.skipNewLine().atNewLine must equal (true)
      walker.skipNewLine().atNewLine must equal (true)
      walker.hasNext must equal (false)
      // now multiple lines
      walker.reset().skipNewLines().hasNext must equal (false)
    }
    "recognize spaces" in {
      val walker = new SubSeqWalker(" \t ")
      walker.skipSpace().skipSpace().skipSpace().hasNext must equal (false)
      walker.reset().skipSpaces().hasNext must equal (false)
    }
    "recognize general whitespace" in {
      val walker = new SubSeqWalker(" \r\n\t\n")
      walker.skipWhitespace().skipWhitespace().at("\t") must equal (true)
      walker.skipWhitespace().skipWhitespace().hasNext must equal (false)
      walker.reset().skipWhitespaces().hasNext must equal (false)
    }
    "recognize blank lines" in {
      val walker = new SubSeqWalker("  \n\n\t\n   Howdy!")
      walker.skipBlankLines().at("   Howdy!") must equal (true)
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
      walker.at("quick") must equal (true)
    }
    "process region exclusions" in {
      val w1 = new SubSeqWalker(pangram, 4, 19)
      val w2 = w1.exclude(6, 12)
      w2.toString must equal ("quick fox")
    }
    "report space number correctly" in {
      val w = new SubSeqWalker("    * item\n")
      w.atSpaces(4) must equal (true)
      w.atSpaces(5) must equal (false)
    }
    "report absolute index" in {
      val walker = new SubSeqWalker(pangram, 4, 19)
      walker.getAbsoluteIndex(4) must equal (8)
    }
  }

  "MultiSeqWalker" - {
    val w1 = new SubSeqWalker(pangram, 0, 10)
    val w2 = new SubSeqWalker(pangram, 16, 35)
    val w3 = new SubSeqWalker(pangram, 10, 16)
    val w4 = new SubSeqWalker(pangram, 40, 44)
    "deliver proper length, chars, and subsequences" in {
      val walker = new MultiSeqWalker(Seq(w1, w2, w3, w4))
      walker.toString must equal ("The quick fox jumps over the brown dog.")
      walker.length must equal (39)
      walker.charAt(2) must equal ('e')
      walker.charAt(9) must equal (' ')
      walker.charAt(10) must equal ('f')
      walker.charAt(31) must equal ('o')
      walker.subSequence(4, 13).toString must equal ("quick fox")
      walker.subSequence(29, 38).toString must equal ("brown dog")
      walker.subSequence(10, 10).toString must equal ("")
      walker.subSequence(0, walker.length).toString must equal (walker.toString)
    }
    "nest correctly" in {
      val m1 = new MultiSeqWalker(Seq(w2, w3))
      val m2 = new MultiSeqWalker(Seq(w1, m1, w4))
      val walker = new SubSeqWalker(m2, 10, 34)
      walker.toString must equal ("fox jumps over the brown")
      walker.length must equal (24)
      walker.charAt(10) must equal ('o')
      walker.subSequence(15, 24).toString must equal ("the brown")
      walker.subSequence(0, 0).toString must equal ("")
      walker.subSequence(0, walker.length).toString must equal (walker.toString)
    }
    "process region exclusions" in {
      val m1 = new MultiSeqWalker(Seq(w1, w2, w3, w4))
      val m2 = m1.exclude(10, 35)
      m2.toString must equal ("The quick dog.")
    }
    "report absolute index" in {
      val walker = new MultiSeqWalker(Seq(w1, w2, w3, w4))
      walker.getAbsoluteIndex(29) must equal (10)
      walker.getAbsoluteIndex(10) must equal (16)
    }
  }

  "LinkDef" - {
    "render link and media" in {
      val ld = new LinkDef("http://eduarea.com", "EduArea Platform")
      ld.toLink("EduArea") must equal ("<a href=\"http://eduarea.com\" " +
          "title=\"EduArea Platform\">EduArea</a>")
      ld.toMedia("") must equal ("<img src=\"http://eduarea.com\" " +
          "title=\"EduArea Platform\" alt=\"\"/>")
    }
  }

  "Selector" - {
    "render attributes string" in {
      val sel = new Selector(testConf, "renaming", Seq("warning", "centered"))
      sel.toString must equal ("{#renaming.warning.centered}")
    }
  }

  "Inline processor" - {
    "distinguish between ampersands and entity references" in {
      val input = "This & that; A&B -- but leave &amp;, &#095; and &#x09a; alone."
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("This &amp; that; A&amp;B &mdash; but leave &amp;, &#095; and &#x09a; alone.")
    }
    "leave inline html tags ensuring correct amps" in {
      val input = "A link: <a href=\"/url?a=b&c=d\" " +
          "title=\"A&amp;B, *text* should stay.\">hello</a>"
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("A link: <a href=\"/url?a=b&amp;c=d\" " +
          "title=\"A&amp;B, *text* should stay.\">hello</a>")
    }
    "leave inline html comments unprocessed" in {
      val input = "This & <!-- this text is *unprocessed* --> that."
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("This &amp; <!-- this text is *unprocessed* --> that.")
    }
    "escape &lt; and &gt; properly" in {
      val input = "A < B; B > A"
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("A &lt; B; B &gt; A")
    }
    "deal with bracing constructs like em and strong" in {
      var input = "Text *_ * _*"
      var w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("Text <strong>_ </strong> _*")
      input = "Some _* nested & not so easy *_ here"
      w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("Some <em><strong> nested &amp; not so easy </strong></em> here")
    }
    "respect backslash escapes" in {
      var input = "Text *_ \\* _*"
      var w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("Text <strong><em> * </em></strong>")
      input = "Some \\_\\* nested & not so easy *_ here"
      w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("Some _* nested &amp; not so easy *_ here")
    }
    "process triple code spans" in {
      val input = "The ```<em>code</em> & _almost_ untouched\\.```."
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("The <code><em>code</em> &amp; _almost_ untouched\\.</code>.")
    }
    "process regular code spans" in {
      val input = "Nested `<html> tags & SGMLs &#095;` are cool."
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("Nested <code>&lt;html&gt; tags &amp; SGMLs &#095;</code> are cool.")
    }
    "respect MathJax formulas" in {
      val input = "%% A < B %% and $$ a<c>d $$"
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("%% A &lt; B %% and $$ a&lt;c&gt;d $$")
    }
    "resolve fragments" in {
      var input = "In text: {{normal}}\nIn code: `{{normal}}`"
      var w = new StringWriter
      new InlineProcessor(w, testConf).process(input)
      w.toString must equal ("In text: normal " +
          "<strong>text</strong> &mdash; <em>process</em> it\n" +
          "In code: <code>normal <strong>text</strong> &mdash; <em>process</em> it</code>")
      input = "In text: {{code}}\nIn code: `{{code}}`"
      w = new StringWriter
      new InlineProcessor(w, testConf).process(input)
      w.toString must equal ("In text: code, &lt;em&gt;a &amp; b&lt;/em&gt;\n" +
          "In code: <code>code, &lt;em&gt;a &amp; b&lt;/em&gt;</code>")
      input = "In text: {{plain}}\nIn code: `{{plain}}`"
      w = new StringWriter
      new InlineProcessor(w, testConf).process(input)
      w.toString must equal ("In text: plain, <em>a &amp; b</em>\n" +
          "In code: <code>plain, <em>a &amp; b</em></code>")
    }
    "process inline links" in {
      var input = "Simple [_inline_ link](/url)."
      var w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("Simple <a href=\"/url\"><em>inline</em> link</a>.")
      input = "Kinda \\[[confusing [\\]() now](/?a=b&c=d)]"
      w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("Kinda [<a href=\"/?a=b&amp;c=d\">confusing []() now</a>]")
    }
    "process inline images" in {
      val input = "Acorn: ![*not* processed & escaped](/acorn.png)"
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("Acorn: <img src=\"/acorn.png\" alt=\"*not* processed &amp; escaped\"/>")
    }
    "process reference links and images" in {
      val input = "[A & B][ea]\n[Lookup `<a>` in book][cx]\n![][acorn]"
      val w = new StringWriter
      new InlineProcessor(w, testConf).process(input)
      w.toString must equal ("<a href=\"http://eduarea.com\" " +
          "title=\"EduArea &amp; Friends\">A &amp; B</a>\n" +
          "<a href=\"http://circumflex.ru\">Lookup <code>&lt;a&gt;</code> in book</a>\n" +
          "<img src=\"http://eduarea.com/img/acorn.png\" title=\"EduArea Acorn Logo\" alt=\"\"/>")
    }
    "process mixed inline-reference links" in {
      val input = "[A & B](ea)\n[Lookup `<a>` in book][cx]\n[](acorn)"
      val w = new StringWriter
      new InlineProcessor(w, testConf).process(input)
      w.toString must equal ("<a href=\"ea\">A &amp; B</a>\n" +
          "<a href=\"http://circumflex.ru\">Lookup <code>&lt;a&gt;</code> in book</a>\n" +
          "<a href=\"acorn\"></a>")
    }
    "process headless links" in {
      val input = "Headless link to [[ea]]"
      val w = new StringWriter
      new InlineProcessor(w, testConf).process(input)
      w.toString must equal ("Headless link to " +
          "<a href=\"http://eduarea.com\" title=\"EduArea &amp; Friends\">" +
          "EduArea &amp; Friends</a>")
    }
    "process basic typographics" in {
      val input = "\"This & that\" -- &quot;(c) (r) (TM)&quot; ..."
      val w = new StringWriter
      new InlineProcessor(w).process(input)
      w.toString must equal ("&laquo;This &amp; that&raquo; &mdash; &laquo;&copy; &reg; &trade;&raquo; &hellip;")
    }
  }

}