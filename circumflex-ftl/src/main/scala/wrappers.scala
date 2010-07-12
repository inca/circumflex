package ru.circumflex.freemarker

import _root_.freemarker.template._
import java.util.Date
import org.apache.commons.beanutils.{MethodUtils, PropertyUtils}
import java.lang.reflect.{Field, Method}
import java.lang.String
import ru.circumflex.core.{WrapperModel, HashModel}

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
    case hash: HashModel => new CircumflexHashWrapper(hash, this)
    case wrapper: WrapperModel => wrap(wrapper.item)
    // Scala base types
    case seq: Seq[Any] => new ScalaSeqWrapper(seq, this)
    case map: scala.collection.Map[Any, Any] => new ScalaMapWrapper(map, this)
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

class ScalaMapWrapper[String,V](val map: scala.collection.Map[String,V], wrapper: ObjectWrapper)
    extends ScalaBaseWrapper(map, wrapper) with TemplateHashModelEx {
  override def get(key: java.lang.String) = wrapper.wrap(
    map.get(key.asInstanceOf[String])
        orElse map.get(key.replaceAll("\\$","_").asInstanceOf[String])
        orElse Some(super.get(key)))
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
    // try property via beanutils
    try {
      return wrapper.wrap(PropertyUtils.getProperty(obj, key))
    } catch { case _ => }
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

class CircumflexHashWrapper(val hash: HashModel, wrapper: ObjectWrapper)
    extends ScalaBaseWrapper(hash, wrapper) with TemplateHashModel {
  override def get(key: String) = wrapper.wrap(hash.get(key))
  override def isEmpty = false
}
