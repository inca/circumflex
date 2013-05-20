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
trait MarkevenRenderer {

  def renderer = this

  // Resolving

  def resolveLink(id: String): Option[LinkDef]

  def resolveMedia(id: String): Option[LinkDef]

  def resolveFragment(id: String): Option[FragmentDef]

  // Basic methods

  def toHtml(cs: CharSequence): String = finalize(processBlocks(cs))

  def toInlineHtml(cs: CharSequence): String = finalize(processInlines(cs))

  def processBlocks(cs: CharSequence) = {
    val w = new StringWriter
    new BlockProcessor(w, renderer).process(cs)
    w.toString
  }

  def processInlines(cs: CharSequence) = {
    val w = new StringWriter
    new InlineProcessor(w, renderer).process(cs)
    w.toString
  }

  // Configuration

  def sanitizer: Sanitizer = DEFAULT_SANITIZER

  def leftQuote = cx.get("markeven.typo.leftQuote")
      .map(_.toString)
      .getOrElse("&laquo;")

  def rightQuote = cx.get("markeven.typo.rightQuote")
      .map(_.toString)
      .getOrElse("&raquo;")

  protected val _scrambler = cx.instantiate[TextScrambler](
    "markeven.scrambler", EmptyTextScrambler)
  def scrambler = _scrambler

  protected val _includeSourceIndex =
    cx.getBoolean("markeven.includeSourceIndex")
        .getOrElse(false)
  def includeSourceIndex = _includeSourceIndex

  protected val _autoAssignIdsPrefix =
    cx.getString("markeven.autoAssignIdsPrefix")
        .getOrElse("")
  def autoAssignIdsPrefix = _autoAssignIdsPrefix

  protected val _stripInvalidXmlChars =
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

/*! The `DelegatingMarkevenRenderer` forwards all configuration-related methods
to specified `delegate`.

It is useful if you wish to override only a couple of specific methods
of `delegate`, leaving all others intact. */
class DelegatingMarkevenRenderer(val delegate: MarkevenRenderer)
    extends MarkevenRenderer {

  def resolveLink(id: String) = delegate.resolveLink(id)
  def resolveMedia(id: String) = delegate.resolveMedia(id)
  def resolveFragment(id: String) = delegate.resolveFragment(id)

  override def sanitizer = delegate.sanitizer

  override def leftQuote = delegate.leftQuote
  override def rightQuote = delegate.rightQuote

  override def scrambler = delegate.scrambler

  override def includeSourceIndex = delegate.includeSourceIndex
  override def autoAssignIdsPrefix = delegate.autoAssignIdsPrefix
  override def stripInvalidXmlChars = delegate.stripInvalidXmlChars
}