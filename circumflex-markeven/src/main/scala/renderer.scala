package ru.circumflex
package markeven

import ru.circumflex._, core._
import collection.mutable.HashMap
import java.io.{Writer, StringWriter}

class Renderer(var conf: MarkevenConf) {
  def toHtml(cs: CharSequence): String = finalize(processBlocks(cs))
  def toInlineHtml(cs: CharSequence): String = finalize(processInlines(cs))

  def blockProcessor(out: Writer): BlockProcessor =
    new BlockProcessor(out, conf)

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

  // Stashing allows certain snippets to be rendered
  // bypassing whitelist sanitizer.
  // Unstashing is NOT recursive.

  def stash(chars: String): String = {
    val key = "{[[{" + randomString(20) + "}]]}"
    _stash += key -> chars
    key
  }

  protected val _stash = new HashMap[String, String]

  def unstashAll(output: String): String = {
    var result = output
    _stash.keys.foreach { k =>
      val v = _stash(k)
      result = result.replace(k, v)
    }
    result
  }

  // Finalization includes sanitization and unstashing (in that order)

  def finalize(output: String): String =
    unstashAll(sanitizer.sanitize(output))
}