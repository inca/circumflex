package ru.circumflex

import core._
import java.io.StringWriter

/*!# The `markeven` Package

Package `markeven` contains the default `MarkevenConf`, which you can set via
`markeven.conf` configuration property and a shortcut method `toHtml`, which
you can use to render markup in a quick-and-dirty fashion:

```
val html = markeven.toHtml("# This is some text in Markeven")
```
*/
package object markeven {

  val conf = cx.instantiate[MarkevenConf]("markeven.conf", EmptyMarkevenConf)

  def toHtml(input: String): String = {
    val w = new StringWriter
    new BlockProcessor(w, conf).process(input)
    w.toString
  }

}