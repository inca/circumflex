package circumflex
package markeven

import core._
import java.io.{StringWriter, Writer}

/*!# Inline processor

The `InlineProcessor` class represents the inline-level Unit of Work.

It accepts two parameters:

  * `out` is the `Writer` to which the output markup is rendered;
  * `renderer` is the `MarkevenRenderer` instance.
*/
class InlineProcessor(val out: Writer,
                      val renderer: MarkevenRenderer = DefaultMarkevenRenderer)
    extends Processor {

  /* The `run` method is an entry point of `InlineProcessor`. */
  def run(walk: Walker) {
    while (walk.hasCurrent)
      inline(walk)
  }

  def inline(walk: Walker) {
    // Escapes
    if (tryBackslashEscape(walk)) return
    // Typographics
    if (tryTypographics(walk)) return
    // Special chars
    if (tryAmp(walk)) return
    if (tryLt(walk)) return
    if (tryGt(walk)) return
    // Now bracing elements, from most special to least special
    if (tryTripleCodeSpan(walk)) return
    if (tryCodeSpan(walk)) return
    if (tryFormula(walk)) return
    if (tryEm(walk)) return
    if (tryStrong(walk)) return
    // Link, media and fragments
    if (tryHeadlessLink(walk)) return
    if (tryFragment(walk)) return
    if (tryMedia(walk)) return
    if (tryLink(walk)) return
    // Now generic characters
    out.write(renderer.scrambler.getSpan)
    flushGeneric(walk)
  }

  /*! Generic characters are spit to the output "as is". */
  def flushGeneric(walk: Walker) {
    val char = walk.current
    if (!(renderer.stripInvalidXmlChars && isInvalidXmlChar(char)))
      out.write(char)
    walk.skip()
  }

  def isInvalidXmlChar(char: Char): Boolean = {
    val code = char.toInt
    (code >= 0x1 && code <= 0x8) ||
        (code >= 0xB && code <= 0xC) ||
        (code >= 0xE && code <= 0x1F) ||
        (code >= 0x7F && code <= 0x84) ||
        (code >= 0x86 && code <= 0x9F) ||
        (code >= 0xFDD0 && code <= 0xFDDF) ||
        (code % 0x10000 == 0xFFFE) ||
        (code % 0x10000 == 0xFFFF)
  }

  /*! Certain chars, usually markers, can be backslash-escaped.
  We should respect them, too */
  def tryBackslashEscape(walk: Walker): Boolean = {
    if (walk.at("\\")) {
      // assume backslash escape
      walk.at(const.backslashEscape) match {
        case Some(m) =>
          out.write(m.group(1))
          walk.skip(m.group(0).length)
        case _ =>
          out.write("\\")
          walk.skip()
      }
      true
    } else false
  }

  /*! Ampersands should be escaped as SGML entities as long as they do not
  represent SGML entities themselves. */
  def tryAmp(walk: Walker): Boolean = {
    if (walk.at("&")) {
      // assume entity reference
      walk.at(const.entityRefefence) match {
        case Some(m) =>
          val s = m.group(0)
          walk.skip(s.length)
          out.write(s)
        case _ =>
          out.write("&amp;")
          walk.skip()
      }
      true
    } else false
  }

  /*! The same logic applies to `<`, we escape it as SGML entity as long as
  it is not part of inline HTML tag or comment. Note that ampersands should be escaped
  even inside HTML tags. */
  def tryLt(walk: Walker): Boolean = {
    if (walk.at("<")) {
      // assume html tag
      walk.at(const.htmlTag) match {
        case Some(m) =>
          val s = m.group(0)
          walk.skip(s.length)
          val w = new SubSeqWalker(s)
          while (w.hasCurrent)
            flushHtmlTag(w)
        case _ =>
          // assume inline HTML comments
          walk.at(const.htmlComment) match {
            case Some(m) =>
              out.write(m.group(0))
              walk.startFrom(m.end)
            case _ =>
              out.write("&lt;")
              walk.skip()
          }
      }
      true
    } else false
  }

  def flushHtmlTag(walk: Walker) {
    if (tryAmp(walk)) return
    if (tryLinkAttr(walk)) return
    flushGeneric(walk)
  }

  def tryLinkAttr(walk: Walker): Boolean =
    walk.at(const.refLink) match {
      case Some(m) =>
        val id = m.group(1)
        renderer.resolveLink(id).orElse(renderer.resolveMedia(id)) match {
          case Some(ld) =>
            out.write(ld.url)
            walk.skip(m.group(0).length)
            true
          case _ =>
            false
        }
      case _ =>
        false
    }

  def flushPlain(walk: Walker) {
    if (tryAmp(walk)) return
    if (tryFragment(walk)) return
    flushGeneric(walk)
  }

  /*! This one does not recognize HTML tags, so is not called in the main `run`.
  It should be called whenever nested HTML markup should be escaped, e.g. in code spans.*/
  def tryEscapeLt(walk: Walker): Boolean = {
    if (walk.at("<")) {
      out.write("&lt;")
      walk.skip()
      true
    } else false
  }

  /*! The `>` symbol is always escaped; therefore this method must be called after
  `tryLt` to preserve inline HTML integrity. */
  def tryGt(walk: Walker): Boolean = {
    if (walk.at(">")) {
      out.write("&gt;")
      walk.skip()
      true
    } else false
  }

  /*! Scans forward to find specified marker, respecting backslashes. */
  def findMarker(walk: Walker, marker: String) = {
    walk.lookahead { it =>
      var found = false
      while (!found && it.hasCurrent) {
        if (it.at("\\" + marker))  // respect backslash escape
          it.skip(marker.length + 1)
        else if (it.at(marker))
          found = true
        else it.skip()
      }
      if (found) Some(it.position)
      else None
    }
  }

  /*! Looks for logical block enclosed into `marker` and executes `inside`,
  passing the block contents into it. Also repositions `walk` at the end
  of such block, if the end-marker exists; otherwise it flushes the marker
  and repositions `walk` at the end of specified `marker`. */
  def tryBracing(walk: Walker,
                 marker: String,
                 inside: Walker => Unit): Boolean = {
    if (walk.at(marker)) {
      walk.skip(marker.length)
      findMarker(walk, marker) match {
        case Some(idx) =>
          val w = new SubSeqWalker(walk, walk.position, idx)
          inside(w)
          walk.startFrom(idx + marker.length)
        case _ =>
          out.write(marker)
      }
      true
    } else false
  }

  /*! Triple code spans are for _hardcore freaks_, their contents is not processed
  at all. We only respect the \``` case to leave three consecutive backticks inside. */
  def tryTripleCodeSpan(walk: Walker): Boolean =
    tryBracing(walk, "```", { w =>
      out.write("<code>")
      while (w.hasCurrent)
        flushPlain(w)
      out.write("</code>")
    })

  /*! The contents of regular code spans is left "as is", preserving escaping
  SGML entity references, `<`, `>` and backslash-escapes. Fragments are also
  rendered inside code spans. */
  def tryCodeSpan(walk: Walker): Boolean =
    tryBracing(walk, "`", { w =>
      out.write("<code>")
      while (w.hasCurrent)
        flushCode(w)
      out.write("</code>")
    })

  def flushCode(walk: Walker) {
    if (tryBackslashEscape(walk)) return
    if (tryAmp(walk)) return
    if (tryEscapeLt(walk)) return
    if (tryGt(walk)) return
    if (tryFragment(walk)) return
    flushGeneric(walk)
  }

  /*! The contents between `%%` and `$$` is interpreted just like in regular code
  spans, except that backslash escapes inside are ignored, and the markers are
  flushed, too. This is to create MathJax-friendly formulas. */

  def tryFormula(walk: Walker): Boolean =
    tryBracing(walk, "%%", { w =>
      out.write("%%")
      while (w.hasCurrent)
        flushFormula(w)
      out.write("%%")
    }) || tryBracing(walk, "$$", { w =>
      out.write("$$")
      while (w.hasCurrent)
        flushFormula(w)
      out.write("$$")
    })

  def flushFormula(walk: Walker) {
    if (tryAmp(walk: Walker)) return
    if (tryEscapeLt(walk: Walker)) return
    if (tryGt(walk: Walker)) return
    if (tryFragment(walk: Walker)) return
    flushGeneric(walk)
  }


  /*! Em and strong are matched reluctantly up their closing character
  (`_` for em, `*` for strong). */

  def tryEm(walk: Walker): Boolean =
    tryBracing(walk, "_", { w =>
      out.write("<em>")
      run(w)
      out.write("</em>")
    })
  def tryStrong(walk: Walker): Boolean =
    tryBracing(walk, "*", { w =>
      out.write("<strong>")
      run(w)
      out.write("</strong>")
    })

  /*! Fragments go in a pair of double braces like `{{id}}`. They are resolved
  using renderer's configuration and processed like regular inlines. */
  def tryFragment(walk: Walker): Boolean =
    if (walk.at("{{")) {
      walk.at(const.fragment) match {
        case Some(m) =>
          val s = m.group(0)
          val id = m.group(1)
          renderer.resolveFragment(id) match {
            case Some(f) if (!renderer.fragmentIds.contains(id)) =>
              renderer.fragmentIds += id
              flushFragment(f)
              renderer.fragmentIds -= id
            case _ =>
              out.write(s)
          }
          walk.skip(s.length)
        case _ =>
          out.write("{{")
          walk.skip(2)
      }
      true
    } else false

  def flushFragment(fragDef: FragmentDef) {
    fragDef.mode match {
      case ProcessingMode.PLAIN => // like triple code span
        val w = new SubSeqWalker(fragDef.body)
        while (w.hasCurrent)
          flushPlain(w)
      case ProcessingMode.CODE => // like regular code span
        val w = new SubSeqWalker(fragDef.body)
        while (w.hasCurrent)
          flushCode(w)
      case _ => // like regular inline
        run(new SubSeqWalker(fragDef.body))
    }
  }

  /*! Two styles of links and media are supported: inline like `[text](url)` and
  referencial like `[text][id]`. The latter ones are resolved using renderer's
  configuration. */
  def tryLink(walk: Walker): Boolean = {
    if (walk.at("[")) {
      resolveLinkDef(walk, false) match {
        case Some((text, linkDef)) =>
          val w = new StringWriter
          new InlineProcessor(w, renderer).process(text)
          linkDef.writeLink(out, w.toString)
        case _ =>
          out.write("[")
          walk.skip()
      }
      true
    } else false
  }

  def tryMedia(walk: Walker): Boolean = {
    if (walk.at("![")) {
      walk.skip()
      resolveLinkDef(walk, true) match {
        case Some((alt, linkDef)) =>
          linkDef.writeMedia(out, wrapHtml(alt))
        case _ =>
          out.write("![")
          walk.skip()
      }
      true
    } else false
  }

  def tryHeadlessLink(walk: Walker): Boolean = {
    if (walk.at("[[")) {
      walk.skip(2)
      val startIdx = walk.position
      var found = false
      while (!found && walk.hasCurrent) {
        if (walk.at("]]")) found = true
        else walk.skip()
      }
      if (found) {
        val id = walk.subSequence(startIdx, walk.position).toString
        renderer.resolveLink(id) match {
          case Some(linkDef) =>
            linkDef.writeLink(out, linkDef.title)
            walk.skip(2)  // skip closing ]]
            return true
          case _ =>
        }
      }
      out.write("[[")
      walk.startFrom(startIdx)
      true
    } else false
  }

  // Resolves link definition and moves walker to the end of that definition
  def resolveLinkDef(walk: Walker, media: Boolean): Option[(String, LinkDef)] = {
    assert(walk.at("["))
    walk.skip()
    val startIdx = walk.position
    while (walk.hasCurrent && !walk.at("]"))
      if (walk.at("\\]")) walk.skip(2)
      else walk.skip()
    if (!walk.hasCurrent) {
      walk.startFrom(startIdx - 1)
      return None
    }
    assert(walk.at("]"))
    val text = walk.subSequence(startIdx, walk.position).toString
    walk.skip()
    walk.at(const.inlineLink) flatMap { m =>
      val s = m.group(0)
      walk.skip(s.length)
      val url = m.group(1)
      Some(text -> new LinkDef(url))
    } orElse {
      walk.at(const.refLink) flatMap { m =>
        val s = m.group(0)
        val id = m.group(1)
        val ld =
          if (media) renderer.resolveMedia(id)
          else renderer.resolveLink(id)
        ld match {
          case Some(linkDef) =>
            walk.skip(s.length)
            Some(text -> linkDef)
          case _ => None
        }
      }
    } orElse {
      // reset the original walker to point to initial "[" in case of any failure
      walk.startFrom(startIdx - 1)
      None
    }
  }

  def tryTypographics(walk: Walker): Boolean = {
    if (walk.at("--")) {
      walk.skip(2)
      out.write("&mdash;")
      return true
    }
    if (walk.at("(r)") || walk.at("(R)")) {
      walk.skip(3)
      out.write("&reg;")
      return true
    }
    if (walk.at("(c)") || walk.at("(C)")) {
      walk.skip(3)
      out.write("&copy;")
      return true
    }
    if (walk.at("(tm)") || walk.at("(TM)")) {
      walk.skip(4)
      out.write("&trade;")
      return true
    }
    if (walk.at("...")) {
      walk.skip(3)
      out.write("&hellip;")
      return true
    }
    if (walk.at("<-")) {
      walk.skip(2)
      out.write("&larr;")
      return true
    }
    if (walk.at("&lt;-")) {
      walk.skip(5)
      out.write("&larr;")
      return true
    }
    if (walk.at("->")) {
      walk.skip(2)
      out.write("&rarr;")
      return true
    }
    if (walk.at("-&gt;")) {
      walk.skip(5)
      out.write("&rarr;")
      return true
    }
    walk.at(const.ty_leftQuote) map { m =>
      walk.startFrom(m.end)
      out.write(renderer.leftQuote)
      return true
    }
    walk.at(const.ty_rightQuote) map { m =>
      walk.startFrom(m.end)
      out.write(renderer.rightQuote)
      return true
    }
    false
  }


}