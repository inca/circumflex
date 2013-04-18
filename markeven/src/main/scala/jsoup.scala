package org.jsoup.safety

import org.jsoup.nodes.{Attribute, Element}

class EmptyHtml5Whitelist extends Whitelist {
  override def isSafeAttribute(tagName: String,
                               el: Element,
                               attr: Attribute) =
    attr.getKey.startsWith("data-") ||
        super.isSafeAttribute(tagName, el, attr)
}
