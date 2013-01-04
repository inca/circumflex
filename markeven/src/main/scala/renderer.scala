package pro.savant.circumflex
package markeven

import pro.savant.circumflex._, core._
import collection.mutable.{HashSet, HashMap}
import java.io.{Writer, StringWriter}

/*! # Markeven Renderer

The `MarkevenRenderer` trait contains configuration parameters and
high-level methods which delegate to `BlockProcessor` and `InlineProcessor`.
Renderers are designed to be shared across different rendering tasks and threads.

In other words, `MarkevenRenderer` can be used to convert multiple texts to HTML
with the same Markeven configuration.

The typical usage is very simple:

``` {.scala}
// Define renderer in configuration or package object
val renderer = new MarkevenRenderer {

  def resolveLink(id: String) = { ... }
  def resolveMedia(id: String) = { ... }
  def resolveFragment(id: String) = { ... }

}

// Render Markeven text
renderer.toHtml("# Hello")    // Returns <h1>Hello</h1>
```
*/
trait MarkevenRenderer { renderer =>

  def sanitizer: Sanitizer = DEFAULT_SANITIZER

  def toHtml(cs: CharSequence): String = finalize(processBlocks(cs))

  def toInlineHtml(cs: CharSequence): String = finalize(processInlines(cs))

  def blockProcessor(out: Writer): BlockProcessor =
    new BlockProcessor(out, renderer)

  protected def processBlocks(cs: CharSequence) = {
    val w = new StringWriter
    blockProcessor(w).process(cs)
    w.toString
  }

  protected def processInlines(cs: CharSequence) = {
    val w = new StringWriter
    blockProcessor(w).inline.process(cs)
    w.toString
  }

  // Configuration

  def leftQuote = cx.get("markeven.typo.leftQuote")
      .map(_.toString).getOrElse("&laquo;")

  def rightQuote = cx.get("markeven.typo.rightQuote")
      .map(_.toString).getOrElse("&raquo;")

  def resolveLink(id: String): Option[LinkDef]

  def resolveMedia(id: String): Option[LinkDef]

  def resolveFragment(id: String): Option[FragmentDef]

  val scrambler = cx.instantiate[TextScrambler](
    "markeven.scrambler", EmptyTextScrambler)

  val _includeSourceIndex =
    cx.getBoolean("markeven.includeSourceIndex")
        .getOrElse(false)
  def includeSourceIndex = _includeSourceIndex

  val _autoAssignIdsPrefix =
    cx.getString("markeven.autoAssignIdsPrefix")
        .getOrElse("")
  def autoAssignIdsPrefix = _autoAssignIdsPrefix

  val _stripInvalidXmlChars =
    cx.getBoolean("markeven.stripInvalidXmlChars")
        .getOrElse(true)
  def stripInvalidXmlChars = _stripInvalidXmlChars

  def fragmentIds = ctx.getAs[HashSet[String]]("markeven.fragmentIds")
      .getOrElse {
    val s = new HashSet[String]
    ctx.update("markeven.fragmentIds", s)
    s
  }

  // Stashing allows certain snippets to be rendered
  // bypassing whitelist sanitizer.
  // Unstashing is NOT recursive.

  def stashStorage = ctx.getAs[HashMap[String, String]]("markeven.stash")
      .getOrElse {
    val h = new HashMap[String, String]
    ctx += "markeven.stash" -> h
    h
  }

  def stash(chars: String): String = {
    val key = "{[[{" + randomString(20) + "}]]}"
    stashStorage += key -> chars
    key
  }

  def unstashAll(output: String): String = {
    val hash = stashStorage
    var result = output
    hash.keys.foreach { k =>
      val v = hash(k)
      result = result.replace(k, v)
    }
    result
  }

  // Finalization includes sanitization and unstashing (in that order)

  def finalize(output: String): String =
    unstashAll(sanitizer.sanitize(output))
}

/*! The default renderer does not resolve any links, media and fragments.

This default implementation is used with method
`markeven.toHtml` (see [[markeven/src/main/scala/package.scala]]) and can
be overridden by setting the `markeven.renderer` configuration parameter. */
object DefaultMarkevenRenderer extends MarkevenRenderer {

  def resolveLink(id: String) = None
  def resolveMedia(id: String) = None
  def resolveFragment(id: String) = None

}