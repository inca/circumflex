package ru.circumflex
package orm

import xml._
import java.io.File

/*!# XML (de)serialization

Circumflex ORM allows you to load graphs of associated records from XML files.
This is very useful for loading test data and exchanging records between databases
with associations preserving (in id-independent style).

Every `Field` capable of (de)serializing itself (from)into XML should extend the
`XmlSerializable` trait. A record can be read from XML format if it contains only
`XmlSerializable` fields.
*/
abstract class XmlSerializable[T, R <: Record[_, R]](
    name: String, record: R, sqlType: String)
    extends Field[T, R](name, record, sqlType) {
  def fromString(str: String): Option[T]
  def toString(value: Option[T]): String =
    value.map(_.toString).getOrElse("")
}

/*! Deployment is a unit of work of XML import tool. It specifies the `prefix` for record
classes resolution, the `onExist` behavior (`keep`, `update` or `recreate`) and whether
record validation is needed before persisting. One deployment corresponds to one transaction,
so each deployment is executed atomically.
*/
class Deployment(val id: String,
                 val prefix: String,
                 val onExist: Deployment.OnExistAction,
                 val validate: Boolean = true,
                 val entries: Seq[Node]) {

  def process() {
    try {
      entries.foreach(e => processNode(e, Nil))
      COMMIT()
    } catch {
      case e: Exception =>
        ROLLBACK()
        throw e
    }
  }

  protected def processNode[R <: Record[Any, R]](
      node: Node,
      parentPath: Seq[Pair[Association[_, _, _], Record[_, _]]]): Record[Any, R] = {
    val cl = pickClass(node)
    var r = cl.newInstance.asInstanceOf[R]
    var update = false
    // Decide, whether a record should be processed, and how exactly.
    if (node.attributes.next != null) {
      val crit = prepareCriteria(r, node)
      crit.unique() match {
        case Some(rec: R) if (onExist == Deployment.Skip || node.child.size == 0) =>
          return rec
        case Some(rec: R) if (onExist == Deployment.Recreate) =>
          crit.mkDelete().execute()
        case Some(rec: R) if (onExist == Deployment.Update) =>
          r = rec
          update = true
        case _ =>
      }
    }
    // If we are still here, let's process the record further.
    // In first place, we set provided parents
    parentPath.foreach { p =>
      if (r.relation.fields.contains(p._1.field))
        r.relation.getField(r, p._1.field.asInstanceOf[Field[Any, R]]).set(p._2.PRIMARY_KEY.value)
    }
    var foreigns: Seq[Pair[Association[_, _, _], Node]] = Nil
    // Secondly, we set fields provided via attributes
    node.attributes.foreach(a => setRecordField(r, a.key, a.value.toString()))
    // Next we process element body
    node.child.foreach {
      case n: Elem => try {
        r.getClass.getMethod(n.label) match {
          case m if (classOf[Field[_, _]].isAssignableFrom(m.getReturnType)) =>
            setRecordField(r, n.label, n.child.mkString.trim)
          case m if (classOf[Association[_, _, _]].isAssignableFrom(m.getReturnType)) =>
            val a = m.invoke(r).asInstanceOf[Association[Any, R, R]]
            val newPath = parentPath ++ List(a -> r)
            val parent = if (n.child.size == 0) {
              val newNode = Elem(null, a.parentRelation.recordClass.getSimpleName, n.attributes, n.scope)
              Some(processNode(newNode, newPath))
            } else n.child.find(_.isInstanceOf[Elem]).map(n => processNode(n, newPath))
            r.relation.getField(r, a.field).set(parent.map(_.PRIMARY_KEY.value))
          case m if (classOf[InverseAssociation[_, _, _, _]].isAssignableFrom(m.getReturnType)) =>
            val a = m.invoke(r).asInstanceOf[InverseAssociation[Any, R, R, Any]].association
            foreigns ++= n.child.filter(_.isInstanceOf[Elem]).map(n => (a -> n))
        }
      } catch {
        case e: NoSuchMethodException =>
          ORM_LOG.warn("Could not process '" + n.label + "' of " + r.getClass)
      }
      case _ =>
    }
    // Now the record is ready to be saved
    if (update)
      if (validate) r.UPDATE() else r.UPDATE_!()
    else
    if (validate) r.INSERT() else r.INSERT_!()
    // Finally, we process the foreigners
    foreigns.foreach(p =>
      processNode(p._2, parentPath ++ List(p._1.asInstanceOf[Association[Any, R, R]] -> r)))
    // And return our record
    r
  }

  protected def pickClass(node: Node): Class[_] = {
    var p = ""
    if (prefix != "") p = prefix + "."
    Class.forName(p + node.label, true, Thread.currentThread().getContextClassLoader)
  }

  protected def setRecordField[R <: Record[_, R]](r: R, k: String, v: String) = {
    val m = r.getClass.getMethod(k)
    if (classOf[Field[_, _]].isAssignableFrom(m.getReturnType)) {    // only scalar fields are accepted
      val field = m.invoke(r).asInstanceOf[Field[Any, R]]
      val value = convertValue(field, v)
      field.set(value)
    }
  }

  protected def prepareCriteria[R <: Record[Any, R]](r: R, n: Node): Criteria[Any, R] = {
    val crit = r.relation.AS("root").criteria
    n.attributes.foreach(a => {
      val k = a.key
      val field = r.relation.getClass.getMethod(k).invoke(r).asInstanceOf[Field[Any, R]]
      val v = convertValue(field, a.value.toString())
      aliasStack.push("root")
      crit.add(field EQ v)
    })
    crit
  }

  protected def convertValue(field: Field[Any, _], v: String): Option[Any] = field match {
    case field: XmlSerializable[Any, _] => field.fromString(v)
    case _ => Some(v)
  }

  override def toString = id match {
    case "" => "deployment@" + hashCode
    case _ => id
  }

}

object Deployment {

  trait OnExistAction
  object Skip extends OnExistAction
  object Update extends OnExistAction
  object Recreate extends OnExistAction

  def readOne(n: Node): Deployment = if (n.label == "deployment") {
    val id = (n \ "@id").text
    val prefix = (n \ "@prefix").text
    val onExist = (n \ "@onExist").text match {
      case "keep" | "ignore" | "skip" => Deployment.Skip
      case "update" => Deployment.Update
      case "recreate" | "delete" | "delete-create" | "overwrite" => Deployment.Recreate
      case _ => Deployment.Skip
    }
    val validate = (n \ "@validate").text match {
      case "false" | "f" | "no" | "off" => false
      case _ => true
    }
    new Deployment(id, prefix, onExist, validate, n.child.filter(n => n.isInstanceOf[Elem]))
  } else throw new ORMException("<deployment> expected, but <" + n.label + "> found.")

  def readAll(n: Node): Seq[Deployment] = if (n.label == "deployments")
    (n \ "deployment").map(n => readOne(n))
  else throw new ORMException("<deployments> expected, but " + n.label + " found.")

}

class DeploymentHelper(f: File) {
  def loadData() {
    Deployment.readAll(XML.loadFile(f)).foreach(_.process())
  }
}
