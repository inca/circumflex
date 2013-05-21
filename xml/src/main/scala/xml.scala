package circumflex
package xml

import java.io._
import javax.xml.stream._
import collection.Iterator

/*! # XML parsing utils

These tiny utils are based on StAX parser from JDK and are used internally
by XML mapping API (see [[/xml/src/main/scala/holder.scala]]).
*/
object xml extends XMLStreamConstants {

  val xmlif: XMLInputFactory = {
    val fac = XMLInputFactory.newFactory
    fac.setProperty("javax.xml.stream.isValidating", false)
    fac.setProperty("javax.xml.stream.isNamespaceAware", false)
    fac.setProperty("javax.xml.stream.isNamespaceAware", false)
    fac.setProperty("javax.xml.stream.isCoalescing", true)
    fac.setProperty("javax.xml.stream.isReplacingEntityReferences", true)
    fac
  }

  def parseStream(in: InputStream)(block: TagIterator => Any) {
    val xmlr = xmlif.createXMLStreamReader(in)
    val it = new TagIterator(xmlr)
    try {
      block(it)
    } finally {
      xmlr.close()
      in.close()
    }
  }

  def parse(file: File)(block: TagIterator => Any) {
    if (!file.isFile) {
      XML_LOG.warn("File " + file.getAbsoluteFile + " does not exist.")
      return
    }
    val is = new FileInputStream(file)
    parseStream(is)(block)
  }

  def parseString(string: String)(block: TagIterator => Any) {
    val is = new ByteArrayInputStream(string.getBytes("UTF-8"))
    parseStream(is)(block)
  }

}

class TagIterator(val reader: XMLStreamReader) {

  protected var currentElem: Option[XmlTag] = None
  protected var lastElem: Option[XmlTag] = None

  def current: XmlTag = currentElem.getOrElse(
    throw new IllegalStateException("No current element in TagIterator."))

  def next(): XmlTag = lastElem match {
    case Some(e) =>
      currentElem = lastElem
      lastElem = None
      e
    case None =>
      if (hasNext) next()
      else throw new IndexOutOfBoundsException("No more elements in TagIterator.")
  }

  def hasNext: Boolean = lastElem match {
    case Some(e) => true
    case None if (reader.hasNext) =>
      val e = reader.next
      if (e == XMLStreamConstants.START_ELEMENT) {
        lastElem = Some(StartTag(reader.getLocalName))
        true
      } else if (e == XMLStreamConstants.END_ELEMENT) {
        lastElem = Some(EndTag(reader.getLocalName))
        true
      } else hasNext
    case None => false
  }

  def skip() {
    XML_LOG.trace("Skipping " + current)
    current match {
      case StartTag(name) =>
        var depth = 1
        do {
          next() match {
            case StartTag(_) =>
              depth += 1
            case EndTag(_) =>
              depth -= 1
          }
        } while (depth > 0)
      case _ =>
    }
  }

  def text: String = {
    currentElem = lastElem
    lastElem = None
    reader.getElementText
  }

  def attr(name: String): Option[String] = {
    val a = reader.getAttributeValue(null, name)
    if (a == null) None
    else Some(a)
  }

  override def toString = currentElem.map(_.toString).getOrElse("---")
}

sealed trait XmlTag {
  def name: String
}

case class StartTag(name: String) extends XmlTag
case class EndTag(name: String) extends XmlTag