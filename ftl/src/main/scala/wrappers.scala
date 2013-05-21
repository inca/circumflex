package circumflex
package freemarker

import _root_.freemarker.template._
import java.util.Date
import org.apache.commons.beanutils.MethodUtils
import java.lang.String
import core._
import scala.collection.Map
import java.lang.reflect.{InvocationTargetException, Modifier, Field, Method}

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
    case seq: Seq[Any] => new ScalaSeqWrapper(seq, this)
    case array: Array[Any] => new ScalaArrayWrapper(array, this)
    case map: Map[_, _] => new ScalaMapWrapper(map.asInstanceOf[Map[Any, Any]], this)
    case it: Iterable[Any] => new ScalaIterableWrapper(it, this)
    case it: Iterator[Any] => new ScalaIteratorWrapper(it, this)
    case str: String => new SimpleScalar(str)
    case date: Date => new ScalaDateWrapper(date, this)
    case num: Number => new SimpleNumber(num)
    case bool: Boolean =>
      if (bool) TemplateBooleanModel.TRUE else TemplateBooleanModel.FALSE
    // Everything else
    case o => new ScalaBaseWrapper(o, this)
  }

}

class ScalaDateWrapper(val date: Date, wrapper: ObjectWrapper)
    extends TemplateDateModel {

  def getDateType = TemplateDateModel.UNKNOWN

  def getAsDate = date

}

class ScalaSeqWrapper[T](val seq: Seq[T], wrapper: ObjectWrapper)
    extends TemplateSequenceModel {

  def get(index: Int) = wrapper.wrap(seq(index))

  def size = seq.size

}

class ScalaArrayWrapper[T](val array: Array[T], wrapper: ObjectWrapper)
    extends TemplateSequenceModel {

  def get(index: Int) = wrapper.wrap(array(index))

  def size = array.length

}

class ScalaMapWrapper(val map: Map[Any, Any], wrapper: ObjectWrapper)
    extends TemplateHashModelEx {

  override def get(key: String) = wrapper.wrap(map.get(key))

  override def isEmpty = map.isEmpty

  def values = new ScalaIterableWrapper(map.values, wrapper)

  val keys = new ScalaIterableWrapper(map.keys, wrapper)

  def size = map.size

}

class ScalaIterableWrapper[T](val it: Iterable[T], wrapper: ObjectWrapper)
    extends TemplateCollectionModel {

  def iterator = new ScalaIteratorWrapper(it.iterator, wrapper)

}

class ScalaIteratorWrapper[T](val it: Iterator[T], wrapper: ObjectWrapper)
    extends TemplateModelIterator with TemplateCollectionModel {

  def next = wrapper.wrap(it.next())

  def hasNext = it.hasNext

  def iterator = this

}

class ScalaMethodWrapper(val target: Any,
                         val methodName: String,
                         val wrapper: ObjectWrapper)
    extends TemplateMethodModel {

  def exec(arguments: java.util.List[_]) = {
    val args = arguments.toArray
    val result = try{
      MethodUtils.invokeMethod(target, methodName, args)
    } catch {
      case e: InvocationTargetException if (e.getCause != null) =>
        throw e.getCause
    }
    wrapper.wrap(result)
  }

}

class ScalaBaseWrapper(val obj: Any, val wrapper: ObjectWrapper)
    extends TemplateHashModel with TemplateScalarModel {

  val objectClass = obj.asInstanceOf[Object].getClass

  private def findMethod(cl: Class[_], name: String): Option[Method] =
    cl.getMethods.toList.find { m =>
      m.getName.equals(name) && Modifier.isPublic(m.getModifiers)
    } match {
      case None if cl != classOf[Object] =>
        findMethod(cl.getSuperclass, name)
      case other => other
    }

  private def findField(cl: Class[_], name: String): Option[Field] =
    cl.getFields.toList.find { f =>
      f.getName.equals(name) && Modifier.isPublic(f.getModifiers)
    } match {
      case None if cl != classOf[Object] => findField(cl.getSuperclass, name)
      case other => other
    }

  def get(key: String): TemplateModel = {
    val o = obj.asInstanceOf[Object]
    if (key.startsWith("$"))
      return wrapper.wrap(null)
    if (resolveFields)
      findField(objectClass, key) match {
        case Some(field) => return wrapper.wrap(field.get(o))
        case _ =>
      }
    if (resolveMethods)
      findMethod(objectClass, key) match {
        case Some(method) if (method.getParameterTypes.length == 0) =>
          return wrapper.wrap(method.invoke(obj))
        case Some(method) =>
          return new ScalaMethodWrapper(obj, method.getName, wrapper)
        case _ =>
      }
    // nothing found
    if (delegateToDefault)
      ObjectWrapper.DEFAULT_WRAPPER.wrap(obj)
    else wrapper.wrap(null)
  }

  def isEmpty = false

  def getAsString = obj.toString
}
