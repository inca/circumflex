package circumflex

import core._

/*!# The `markeven` package

Package object `markeven` contains the default `MarkevenRenderer` instance,
which you can configure via `markeven.renderer` configuration parameter,
the default sanitizer configured via the `markeven.sanitizer` parameter
and a shortcut method `toHtml`, which you can use to render markup
in a quick-and-dirty fashion:

``` {.scala}
markeven.toHtml("# Hello")  // The result is <h1>Hello</h1>
```
*/
package object markeven {

  val DEFAULT_RENDERER = cx.instantiate[MarkevenRenderer](
    "markeven.renderer", DefaultMarkevenRenderer)

  val DEFAULT_SANITIZER = cx.instantiate[Sanitizer](
    "markeven.sanitizer", DefaultSanitizer)

  def toHtml(input: CharSequence) = DEFAULT_RENDERER.toHtml(input)

}