package circumflex
package markeven

import org.jsoup._, safety._, parser._
import org.jsoup.nodes.{Entities, Document}

/*! # Sanitizing HTML output

Circumflex Markeven uses [Jsoup](http://jsoup.org) to perform HTML sanitizing
(stripping unsafe output).

The default sanitizer is configured by setting the `markeven.sanitizer`
configuration property. It is used with all `MarkevenRenderer` instances, but
can be overridden in concrete implementations.
*/
trait Sanitizer {
  def sanitize(html: String): String
  def stripText(html: String): String
}

trait JsoupSanitizer extends Sanitizer {
  def cleaner: Cleaner
  def parse(html: String): Document = {
    val dirty = Parser.parseBodyFragment(html, "")
    val clean = cleaner.clean(dirty)
    clean.outputSettings.prettyPrint(false)
    clean.outputSettings.escapeMode(Entities.EscapeMode.xhtml)
    clean
  }
  def sanitize(html: String) = parse(html).body.html
  def stripText(html: String) = parse(html).body.text
}

// Reference implementation

class DefaultSanitizer extends JsoupSanitizer {
  val inlineTags = List("a", "span", "em", "strong", "ins", "del", "mark", "sub", "sup",
    "dfn", "kbd", "var", "cite", "code", "samp", "abbr", "acronym", "label", "img",
    "i", "u", "s", "b", "q", "tt", "nobr")
  val tableTags = List("table", "tbody", "thead", "tfoot", "tr", "td", "th", "caption",
    "col", "colgroup")
  val emptyTags = List("br", "wbr", "hr")
  val blockTags = List("form", "address", "blockquote", "body", "div", "ul", "ol", "li", "p", "dl",
    "dd", "dt", "pre", "h1", "h2", "h3", "h4", "h5", "h6", "legend", "fieldset",
    "audio", "video", "source", "section", "article", "aside")
  val formTags = List("input", "select", "option", "textarea", "button")
  val allTags = inlineTags ++ tableTags ++ emptyTags ++ blockTags ++ formTags

  val whitelist = new EmptyHtml5Whitelist

  allTags.foreach { t =>
    whitelist.addTags(t)
    whitelist.addAttributes(
      t, "id", "class", "style", "title", "data-source-index")
  }

  tableTags.foreach { t =>
    whitelist.addAttributes(t, "align", "valign", "width", "colspan", "rowspan")
  }

  formTags.foreach { t =>
    whitelist.addAttributes(t, "type", "name", "value", "size", "form", "autofocus",
      "title", "checked", "disabled", "placeholder", "required", "min", "max",
      "readonly", "multiple", "maxlength", "src", "selected", "tabindex", "maxlength")
  }

  whitelist.addAttributes("a", "href", "rel", "target", "name", "tabindex")
  whitelist.addAttributes("img", "src", "alt", "align", "width", "height", "vspace")
  whitelist.addAttributes("label", "for")
  whitelist.addAttributes("textarea", "cols", "rows", "wrap")

  val cleaner = new Cleaner(whitelist)
}

object DefaultSanitizer extends DefaultSanitizer