package ru.circumflex.me

import java.util.regex._

class LinkDefinition(val url: String, val title: String)

class MarkevenContext(var protector: Protector = new Protector,
                      var links: Map[String, LinkDefinition] = Map())

class MarkevenProcessor(val ctx: MarkevenContext = new MarkevenContext) {
  def hashHtmlBlocks(s: StringEx): StringEx = s.replaceIndexed(regexes.inlineHtmlStart, m => {
    var startIdx = m.start
    var endIdx = 0
    if (m.group(2) != null) {
      // self-closing tag, escape as is
      endIdx = m.end
    } else {
      // find end-index of matching closing tag
      val tagName = m.group(1)
      // following regex will have `group(1) == null` for closing tags;
      // `group(2)` determines if a tag is self-closing.
      val tm = Pattern.compile("(<" + tagName + "\\b.*?(/)?>)|(</" + tagName + "\\s*>)",
        Pattern.CASE_INSENSITIVE).matcher(s.buffer)
      var depth = 1
      var idx = m.end
      while (depth > 0 && idx < s.length && tm.find(idx)) {
        if (tm.group(1) == null) depth -= 1        // closing tag
        else if (tm.group(2) == null) depth += 1   // opening tag
        idx = tm.end
      }
      endIdx = idx
    }
    // add to protector and replace
    val key = ctx.protector.addToken(s.buffer.subSequence(startIdx, endIdx))
    ("\n" + key + "\n", endIdx)
  })
}

class Block(raw: StringEx) {

}

