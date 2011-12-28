package ru.circumflex
package xml

import core._, cache._
import java.lang.reflect.Modifier
import java.io._
import javax.xml.stream._
import org.apache.commons.io.FileUtils
import collection.Iterator

// Homegrown minimalistic XML (de)serialization library

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

class TagIterator(val reader: XMLStreamReader) extends Iterator[XmlTag] {
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

  override def toString() = ""
}

trait XmlTag {
  def name: String
}
case class StartTag(name: String) extends XmlTag
case class EndTag(name: String) extends XmlTag

trait Holder {
  def elemName: String
  var parent: Option[ElemHolder] = None

  def accept(it: TagIterator): Boolean = it.current.name == elemName

  def toXml: String = writeXml(0)
  def writeXml(indent: Int): String
  def readXml(parent: Option[ElemHolder], it: TagIterator): this.type = {
    this.parent = parent
    readXml(it)
  }
  def readXml(it: TagIterator): this.type

  def loadFrom(file: File) {
    xml.parse(file) { it =>
      XML_LOG.trace("Loading " + getClass.getSimpleName + " instance from " + file.getCanonicalPath)
      it.next()
      readXml(it)
    }
  }
  def saveTo(file: File) {
    XML_LOG.trace("Saving " + getClass.getSimpleName + " instance to " + file.getCanonicalPath)
    FileUtils.writeStringToFile(file, "<?xml version=\"1.0\"?>\n" + toXml, "UTF-8")
  }
  def loadString(string: String) {
    xml.parseString(string) { it =>
      XML_LOG.trace("Loading " + getClass.getSimpleName + " instance from string.")
      it.next()
      readXml(it)
    }
  }

  def canEqual(that: Any): Boolean = that match {
    case h: Holder =>
      h.getClass == this.getClass && h.parent == this.parent
    case _ => false
  }
  override def equals(that: Any): Boolean = that match {
    case h: Holder =>
      h.canEqual(this) && h.elemName == this.elemName
    case _ => false
  }
  override def hashCode: Int = parent.hashCode * 31 + elemName.hashCode
}

class AttrHolder(val elemName: String) extends Holder with Container[String] {
  addSetter(s => escapeHtml(s))
  def writeXml(indent: Int): String = value.map(s => " " + elemName + "=\"" + s + "\"").getOrElse("")
  def readXml(it: TagIterator): this.type = {
    it.attr(elemName).map(a => set(a))
    this
  }
  override def toString = value.getOrElse("")
}

trait ElemHolder extends Holder {
  def findHolders[H <: Holder](holderClass: Class[H]): Seq[H] =
    findHolders(this.getClass, holderClass)

  def attr(elemName: String): AttrHolder = new AttrHolder(elemName)

  private def findHolders[H <: Holder](containerClass: Class[_], holderClass: Class[H]): Seq[H] = {
    val parentHolders: Seq[H] =
      if (containerClass != classOf[Any]) findHolders(containerClass.getSuperclass, holderClass) else Nil
    val thisHolders: Seq[H] = containerClass.getDeclaredFields
        .filter(f => !Modifier.isTransient(f.getModifiers))
        .flatMap(f => try Some(containerClass.getMethod(f.getName)) catch { case _ => None })
        .filter(m => holderClass.isAssignableFrom(m.getReturnType))
        .map(m => m.invoke(this).asInstanceOf[H])
    parentHolders ++ thisHolders
  }

  def findAttrs: Seq[AttrHolder] = findHolders(classOf[AttrHolder])

  def writeAttrs(indent: Int) =
    findAttrs.map(_.writeXml(indent)).filter(_ != "")
        .mkString("\n " + (" " * elemName.length) + ("  " * indent))

}

trait TextHolder extends ElemHolder with Container[String] {
  addSetter(s => escapeHtml(s))
  def readXml(it: TagIterator): this.type = {
    if (accept(it)) {
      findAttrs.foreach(a => a.readXml(it))
      set(it.text)
    }
    this
  }
  def writeXml(indent: Int): String = ("  " * indent) + "<" + elemName +
      writeAttrs(indent) + "><![CDATA[" +
      value.getOrElse("") + "]]></" + elemName + ">"
  override def toString = value.getOrElse("")
}

trait ListHolder[T <: ElemHolder] extends ElemHolder with Container[Seq[T]] {
  def children: Seq[T] = getOrElse(Nil)
  def addChild(child: T): this.type = {
    child.parent = Some(this)
    set(getOrElse(Nil) ++ List(child))
    this
  }
  def readChild(it: TagIterator): Option[T]
  def readXml(it: TagIterator): this.type = {
    if (accept(it)) {
      set(Nil)
      findAttrs.foreach(a => a.readXml(it))
      it.takeWhile(_ != EndTag(elemName)) foreach {
        case t: StartTag => readChild(it) map {
          c =>
            c.readXml(it)
            addChild(c)
        }
        case _ =>
      }
    }
    this
  }
  def writeXml(indent: Int): String = {
    var result = ("  " * indent) + "<" + elemName + writeAttrs(indent)
    val children = getOrElse(Nil)
    if (children.size == 0) result += "/>"
    else result += ">\n" + children.map(_.writeXml(indent + 1)).mkString("\n") +
        "\n" + ("  " * indent) + "</" + elemName + ">"
    result
  }
  def delete(child: T) {
    set(children.filter(_ != child))
  }
}

trait StructHolder extends ElemHolder {

  def findElems: Seq[ElemHolder] = findHolders(classOf[ElemHolder])

  def text(n: String): TextHolder = new TextHolder {
    def elemName = n
  }

  def writeXml(indent: Int): String = {
    var result = ("  " * indent) + "<" + elemName + writeAttrs(indent)
    val children = findElems
    if (children.size == 0) result += "/>"
    else result += ">\n" + children.map(_.writeXml(indent + 1)).mkString("\n") +
        "\n" + ("  " * indent) + "</" + elemName + ">"
    result
  }
  def readXml(it: TagIterator): this.type = {
    if (accept(it)) {
      findAttrs.foreach(a => a.readXml(it))
      val elems = findElems
      it.takeWhile(_ != EndTag(elemName)) foreach {
        case StartTag(n) => elems.find(_.elemName == n).map(_.readXml(Some(this), it))
        case _ => // should only occur if XML contains redundant tags
      }
    }
    this
  }
}

trait XmlFile extends ElemHolder with Cached {
  def descriptorFile: File
  def exists = descriptorFile.isFile
  def expired = exists && (descriptorFile.lastModified > createdAt.getTime)
  def save() {
    saveTo(descriptorFile)
    invalidate()
  }
  def load(): this.type = {
    reclaim()
    loadFrom(descriptorFile)
    this
  }
}

trait XmlSingleton extends XmlFile {
  invalidate()
  def touch() {
    if (!isValid) load()
  }
}