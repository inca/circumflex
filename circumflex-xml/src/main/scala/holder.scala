package ru.circumflex
package xml

import core._, cache._
import java.lang.reflect.Modifier
import org.apache.commons.io.{IOUtils, FileUtils}
import java.io._
import collection.mutable.ListBuffer

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

  def loadFrom(file: File): this.type = {
    xml.parse(file) { it =>
      XML_LOG.trace("Loading " + getClass.getSimpleName + " instance from " + file.getCanonicalPath)
      it.next()
      readXml(it)
    }
    this
  }

  def loadFrom(is: InputStream): this.type = {
    xml.parseStream(is) { it =>
      XML_LOG.trace("Loading " + getClass.getSimpleName + " instance from stream.")
      it.next()
      readXml(it)
    }
    this
  }

  def loadString(string: String): this.type = {
    xml.parseString(string) { it =>
      XML_LOG.trace("Loading " + getClass.getSimpleName + " instance from string.")
      it.next()
      readXml(it)
    }
    this
  }

  def saveTo(file: File): this.type = {
    XML_LOG.trace("Saving " + getClass.getSimpleName +
        " instance to " + file.getCanonicalPath)
    FileUtils.writeStringToFile(file, "<?xml version=\"1.0\"?>\n" + toXml, "UTF-8")
    this
  }

  def saveTo(out: OutputStream): this.type = {
    XML_LOG.trace("Saving " + getClass.getSimpleName + " instance to stream.")
    IOUtils.write("<?xml version=\"1.0\"?>\n" + toXml, out, "UTF-8")
    this
  }

}

trait ValHolder[T]
    extends Holder
    with Container[T]
    with Equals {

  def canEqual(that: Any) = that match {
    case holder: ValHolder[T] =>
      holder.elemName == this.elemName && holder.parent == this.parent
    case _ => false
  }

  override def equals(that: Any) = that match {
    case holder: ValHolder[T] =>
      this.canEqual(holder) && holder.value == this.value
    case _ => false
  }

  override def hashCode = elemName.hashCode * 31 + value.hashCode

  override def toString = value.map(_.toString).getOrElse("")
}

class AttrHolder(val elemName: String)
    extends ValHolder[String] {

  addSetter(s => escapeHtml(s))

  def writeXml(indent: Int): String = value.map(s =>
    " " + elemName + "=\"" + s + "\"").getOrElse("")

  def readXml(it: TagIterator): this.type = {
    it.attr(elemName).map(a => set(a))
    this
  }
}

trait ElemHolder extends Holder {

  def findHolders[H <: Holder](holderClass: Class[H]): Seq[H] =
    findHolders(this.getClass, holderClass)

  def attr(elemName: String): AttrHolder = new AttrHolder(elemName)

  private def findHolders[H <: Holder](containerClass: Class[_], holderClass: Class[H]): Seq[H] = {
    val parentHolders: Seq[H] =
      if (containerClass != classOf[Any])
        findHolders(containerClass.getSuperclass, holderClass) else Nil
    val thisHolders: Seq[H] = containerClass.getDeclaredFields
        .filter(f => !Modifier.isTransient(f.getModifiers))
        .flatMap(f => try {
      Some(containerClass.getMethod(f.getName))
    } catch {
      case e: Exception => None
    })
        .filter(m => holderClass.isAssignableFrom(m.getReturnType))
        .map(m => m.invoke(this).asInstanceOf[H])
    parentHolders ++ thisHolders
  }

  def findAttrs: Seq[AttrHolder] = findHolders(classOf[AttrHolder])

  def writeAttrs(indent: Int) =
    findAttrs.map(_.writeXml(indent)).filter(_ != "")
        .mkString("\n " + (" " * elemName.length) + ("  " * indent))

}

trait TextHolder
    extends ValHolder[String]
    with ElemHolder {

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
}

trait ListHolder[T <: ElemHolder]
    extends ElemHolder {

  val children = new ListBuffer[T]

  def add(child: T): this.type = {
    child.parent = Some(this)
    children += child
    this
  }

  def read: String => T

  def readXml(it: TagIterator): this.type = {
    if (accept(it)) {
      children.clear()
      findAttrs.foreach(a => a.readXml(it))
      it.takeWhile(_ != EndTag(elemName)) foreach {
        case t: StartTag =>
          try {
            val child = read(t.name)
            child.readXml(it)
            add(child)
          } catch {
            case e: MatchError =>
              XML_LOG.warn("Unexpected element <" + t.toString + ">. " +
                  "Please check `readChild` or XML input.")
          }
        case _ =>
      }
    }
    this
  }

  def writeXml(indent: Int): String = {
    var result = ("  " * indent) + "<" + elemName + writeAttrs(indent)
    val chldrn = children
    if (children.size == 0) result += "/>"
    else result += ">\n" + children.map(_.writeXml(indent + 1)).mkString("\n") +
        "\n" + ("  " * indent) + "</" + elemName + ">"
    result
  }

  def delete(child: T) {
    children -= child
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
        case StartTag(n) =>
          elems.find(_.elemName == n).map(_.readXml(Some(this), it))
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