package pro.savant.circumflex
package xml

import core._, cache._
import java.lang.reflect.Modifier
import org.apache.commons.io.{IOUtils, FileUtils}
import java.io._
import collection.mutable.ListBuffer

/*! # XML mapping API

The `Holder` trait is the root of XML mapping components.

It contains:

  * _serialization_ methods exporting component into XML representation:

    * `toXml` returns an XML string;
    * `saveTo` writes XML to specified `File` or `OutputStream`;

  * _deserialization_ methods for reading component from its XML representation:

    * `loadString` reads from specified XML string;
    * `loadFrom` reads specified XML `File` or `InputStream`;

  * mapping methods to provide component with specific information on how to
    serialize and deserialize data:

    * `elemName` must be implemented to return the name of the element
      (or attribute);

    * `accept` probes specified `TagIterator` to determine whether its
      current tag can be used to parse this component;

    * `readXml` and `writeXml` methods, implemented in `ElemHolder` and `AttrHolder`.
*/
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

/*! ## Mapping text

Text variables can be mapped to XML using either attributes or
CDATA text elements.

Both of them implement the `Container[String]` trait
(see [[/core/src/main/scala/model.scala]]) and, therefore,
encapsulate mutable `Option[String]`.
*/
trait StringHolder
    extends Holder
    with Container[String]
    with Coercion
    with Equals {

  addSetter(wrapHtml(_))

  def canEqual(that: Any) = that match {
    case holder: StringHolder =>
      holder.elemName == this.elemName && holder.parent == this.parent
    case _ => false
  }

  override def equals(that: Any) = that match {
    case holder: StringHolder =>
      this.canEqual(holder) && holder.value == this.value
    case _ => false
  }

  override def hashCode = elemName.hashCode * 31 + value.hashCode

  override def toString = value.map(_.toString).getOrElse("")
}


class AttrHolder(val elemName: String)
    extends StringHolder {

  def writeXml(indent: Int): String = value.map(s =>
    " " + elemName + "=\"" + s + "\"").getOrElse("")

  def readXml(it: TagIterator): this.type = {
    it.attr(elemName).map(a => set(a))
    this
  }
}

/*! ## Mapping XML elements

There are three types of XML elements available for mapping:

  * _structural elements_ for general-purpose objects
    which can contain attributes and other elements;

  * _text CDATA elements_ for storing text values and, optionally,
    attributes;

  * _list elements_ parameterized with another element type for storing
    collections of another elements.

All element holders must implement `elemName` and provide the `attr` method
to create an attribute definition.
*/
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
    extends StringHolder
    with ElemHolder {

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

/*! The `StructHolder` defines additional method `text` to create
a text element definition.

Try this example:

``` {.scala}
class Book extends StructHolder {

  def elemName = "book"

  val isbn = attr("isbn")

  val title = text("title")

  val author = text("author")

}

// Deserialization

val book = new Book().loadString("""
<book isbn="978-2-070-61275-8">
  <title><![CDATA[Le Petit Prince]]></title>
  <author><![CDATA[Antoine de Saint-Exupéry]]></author>
</book>
""")

// Serialization

book.toXml
```
*/
trait StructHolder extends ElemHolder {

  def findElems: Seq[ElemHolder] =
    findHolders(classOf[ElemHolder])
        .filter(_ != null)

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
          elems.find(_.accept(it)).map(_.readXml(Some(this), it))
        case _ => // should only occur if XML contains redundant tags
      }
    }
    this
  }

}

/*! The `ListHolder[T]` trait defines an abstract method `read` which
is used to create instances of `T` at parse time. It should be implemented
with the function `String => T` which maps tag names to new `T` instances.

Use `children` and `setChildren` to access the collection of elements
encapsulated by `ListHolder`.

You can create robust type hierarchies combining `ListHolder`
together with `StructHolder`.

Try following example:

``` {.scala}
trait Shape extends ElemHolder

class Circle extends Shape with StructHolder {
  def elemName = "circle"
}

class Rectangle extends Shape with StructHolder {
  def elemName = "rectangle"
}

class CompoundShape extends Shape with ListHolder[Shape] {
  def elemName = "composition"

  def read = {
    case "circle" => new Circle
    case "rectangle" => new Rectangle
    case "composition" => new CompoundShape
  }
}

// Deserialization

val myShape = new CompoundShape().loadString("""
<composition>
  <circle/>
  <circle/>
  <rectangle/>
  <composition>
    <rectangle/>
    <composition>
      <circle/>
      <circle/>
    </composition>
  </composition>
</composition>
""")

// Serialization

myShape.toXml

// Traverse all elements (depth-first)

def printShape(shape: Shape, indent: Int) {
  val i = "\t" * indent
  shape match {
    case comp: CompoundShape =>
      println(i + "composition:")
      comp.children.foreach(c => printShape(c, indent + 1))
    case _ =>
      println(i + shape.elemName)
  }
}

printShape(myShape, 0)
```

The example output should be:

```
composition:
	circle
	circle
	rectangle
	composition:
		rectangle
		composition:
			circle
			circle
```

*/
trait ListHolder[T <: ElemHolder]
    extends ElemHolder {

  val children = new ListBuffer[T]

  def add(child: T) {
    child.parent = Some(this)
    children += child
  }

  def setChildren(children: Seq[T]) {
    this.children.clear()
    this.children ++= children
  }

  def read: String => T

  def readXml(it: TagIterator): this.type = {
    if (accept(it)) {
      val tag = it.current
      children.clear()
      findAttrs.foreach(a => a.readXml(it))
      it.takeWhile(_ != EndTag(tag.name)) foreach {
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
    if (children.size == 0) result += "/>"
    else result += ">\n" + children.map(_.writeXml(indent + 1)).mkString("\n") +
        "\n" + ("  " * indent) + "</" + elemName + ">"
    result
  }

  def delete(child: T) {
    children -= child
  }

}

/*! ## Working with XML files

The `XmlFile` trait provides convenient way to read and write XML files.

It is usually convenient to work with XML-based configurations using
`CacheCell`s, just like we do in following example:

``` {.scala}
class MyConf extends StructHolder with XmlFile {

  def elemName = "myconf"

  def descriptorFile = new File("/path/to/conf.xml")

  val setting1 = attr("setting1")
  val setting2 = attr("setting2")

  object misc extends ListHolder[MiscSetting] {
    ...
  }

}

package object myapp {

  val _conf = new CacheCell[MyConf](new MyConf().load())
  def conf = _conf.get

}

This way every time the `conf` method is used, the `/path/to/conf.xml` file
is checked for modifications using its last modified date, making configuration
hot-reloadable without any additional efforts.
```
*/
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
