package ru.circumflex.freemarker

import freemarker.template._
import java.util.Date
import org.apache.commons.beanutils.MethodUtils
import java.lang.reflect.{Field, Method}
import java.lang.String
import ru.circumflex.core.Wrapper
import scala.collection.Map
import scala.xml._

class ScalaObjectWrapper extends ObjectWrapper {
  override def wrap(obj: Any): TemplateModel = obj match {
  // Basic types
    case null => null
    case option: Option[Any] => option match {
      case Some(o) => wrap(o)
      case _ => null
    }
    case model: TemplateModel => model
    // Circumflex model types
    case wrapper: Wrapper[_] => wrap(wrapper.item)
    // Scala base types
    case xml: NodeSeq => new ScalaXmlWrapper(xml, this)
    case seq: Seq[Any] => new ScalaSeqWrapper(seq, this)
    case map: Map[Any, Any] => new ScalaMapWrapper(map, this)
    case it: Iterable[Any] => new ScalaIterableWrapper(it, this)
    case it: Iterator[Any] => new ScalaIteratorWrapper(it, this)
    case str: String => new SimpleScalar(str)
    case date: Date => new SimpleDate(date, TemplateDateModel.UNKNOWN)
    case num: Number => new SimpleNumber(num)
    case bool: Boolean => if (bool) TemplateBooleanModel.TRUE else TemplateBooleanModel.FALSE
    // Everything else
    case obj => new ScalaBaseWrapper(obj, this)
  }
}

class ScalaSeqWrapper[T](val seq: Seq[T], wrapper: ObjectWrapper)
    extends ScalaBaseWrapper(seq, wrapper) with TemplateSequenceModel {
  def get(index: Int) = wrapper.wrap(seq(index))
  def size = seq.size
}

class ScalaMapWrapper(val map: Map[Any, Any], wrapper: ObjectWrapper)
    extends ScalaBaseWrapper(map, wrapper) with TemplateHashModelEx {
  override def get(key: String) = wrapper.wrap(
    map.get(key).orElse(Some(super.get(key))))
  override def isEmpty = map.isEmpty
  def values = new ScalaIterableWrapper(map.values, wrapper)
  val keys = new ScalaIterableWrapper(map.keys, wrapper)
  def size = map.size
}

class ScalaIterableWrapper[T](val it: Iterable[T], wrapper: ObjectWrapper)
    extends ScalaBaseWrapper(it, wrapper) with TemplateCollectionModel {
  def iterator = new ScalaIteratorWrapper(it.iterator, wrapper)
}

class ScalaIteratorWrapper[T](val it: Iterator[T], wrapper: ObjectWrapper)
    extends ScalaBaseWrapper(it, wrapper) with TemplateModelIterator with TemplateCollectionModel {
  def next = wrapper.wrap(it.next)
  def hasNext = it.hasNext
  def iterator = this
}

class ScalaMethodWrapper(val target: Any,
                         val methodName: String,
                         val wrapper: ObjectWrapper)
    extends TemplateMethodModel {
  def exec(arguments: java.util.List[_]) =
    wrapper.wrap(MethodUtils.invokeMethod(target, methodName, arguments.toArray))
}

class ScalaXmlWrapper(val node: NodeSeq, val wrapper: ObjectWrapper) extends TemplateNodeModel
    with TemplateHashModel with TemplateSequenceModel with TemplateScalarModel {
  // as node
  def children: Seq[Node] = node match {
    case node: Elem => node.child.flatMap {
      case e: Elem => Some(e)
      case a: Attribute => Some(a)
      case t: Text => if (t.text.trim == "") None else Some(t)
      case _ => None
    }
    case _ => Nil
  }
  def getNodeNamespace: String = node match {
    case e: Elem => e.namespace
    case _ => ""
  }
  def getNodeType: String = node match {
    case e: Elem => "element"
    case t: Text => "text"
    case a: Attribute => "attribute"
    case _ => null
  }
  def getNodeName: String = node match {
    case e: Elem => e.label
    case _ => null
  }
  def getChildNodes: TemplateSequenceModel = new ScalaSeqWrapper[Node](children, wrapper)
  // due to immutability of Scala XML API, nodes are unaware of their parents.
  def getParentNode: TemplateNodeModel = new ScalaXmlWrapper(null, wrapper)
  // as hash
  def isEmpty: Boolean = node.size == 0
  def get(key: String): TemplateModel = {
    val children = node \ key
    if (children.size == 0) wrapper.wrap(None)
    if (children.size == 1) wrapper.wrap(children(0))
    else wrapper.wrap(children)
  }
  // as sequence
  def size: Int = node.size
  def get(index: Int): TemplateModel = new ScalaXmlWrapper(node(index), wrapper)
  // as scalar
  def getAsString: String = node.text
}

class ScalaBaseWrapper(val obj: Any, val wrapper: ObjectWrapper) extends TemplateHashModel with TemplateScalarModel {

  val objectClass = obj.asInstanceOf[Object].getClass

  private def findMethod(cl: Class[_], name: String): Option[Method] =
    cl.getMethods.toList.find(_.getName.equals(name)) match {
      case None if cl != classOf[Object] => findMethod(cl.getSuperclass, name)
      case other => other
    }

  private def findField(cl: Class[_], name: String): Option[Field] =
    cl.getFields.toList.find(_.getName.equals(name)) match {
      case None if cl != classOf[Object] => findField(cl.getSuperclass, name)
      case other => other
    }

  def get(key: String): TemplateModel = {
    val o = obj.asInstanceOf[Object]
    // try field
    findField(objectClass, key) match {
      case Some(field) => return wrapper.wrap(field.get(o))
      case _ =>
    }
    // try method
    findMethod(objectClass, key) match {
      case Some(method) if (method.getParameterTypes.length == 0) =>
        return wrapper.wrap(method.invoke(obj))
      case Some(method) =>
        return new ScalaMethodWrapper(obj, method.getName, wrapper)
      case _ =>
    }
    return wrapper.wrap(null)
  }

  def isEmpty = false

  def getAsString = obj.toString
}
