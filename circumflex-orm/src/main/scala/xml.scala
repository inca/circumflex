package ru.circumflex.orm

import xml._
import java.io.File
import ORM._

// ## XML (de)serialization

/**
 * A trait for data holders that are capable of (de)serializing themselves (from)into
 * XML. 
 */
trait XmlSerializable[T] {
  def to(value: T): String
  def from(string: String): T
}

/**
 * Deployment is a unit of work of XML import tool. It specifies the prefix
 * for record classes resolution, as well as the behavior, if certain records
 * already exist.
 */
class Deployment(val id: String,
                 val prefix: String,
                 val onExist: Deployment.OnExistAction,
                 val entries: Seq[Node],
                 val validate: Boolean = true) {

  def process(): Unit = try {
    entries.foreach(e => processNode(e, Nil))
    tx.commit
  } catch {
    case e =>
      tx.rollback
      throw e
  }

  protected def processNode[R <: Record[R]](
      node: Node,
      parentPath: Seq[Pair[Association[_, _], Record[_]]]): Record[R] = {
    val cl = pickClass(node)
    var r = cl.newInstance.asInstanceOf[R]
    // Decide, whether a record should be processed, and how exactly.
    if (node.attributes.next != null) {
      val crit = prepareCriteria(r, node)
      crit.unique match {
        case Some(rec: R) if (onExist == Deployment.Skip || node.child.size == 0) =>
          return rec
        case Some(rec: R) if (onExist == Deployment.Recreate) =>
          crit.mkDelete.execute()
        case Some(rec: R) if (onExist == Deployment.Update) =>
          r = rec
        case _ =>
      }
    }
    // If we are still here, let's process the record further: set parents, attributes,
    // subelements and foreigners.
    parentPath.foreach(p =>
      if (r._fields.contains(p._1.field)) r.setField(p._1.field, p._2.id.getValue))
    var foreigns: Seq[Pair[Association[_, _], Node]] = Nil
    node.attributes.foreach(a => setRecordField(r, a.key, a.value.toString))
    node.child.foreach {
      case n: Elem => try {
        r.getClass.getMethod(n.label) match {
          case m if (classOf[Field[_]].isAssignableFrom(m.getReturnType)) =>
            setRecordField(r, n.label, n.text.trim)
          case m if (classOf[Association[_, _]].isAssignableFrom(m.getReturnType)) =>
            n.child.find(_.isInstanceOf[Elem]) match {
              case Some(n) =>
                val a = m.invoke(r).asInstanceOf[Association[R, R]]
                val parent = processNode(n, parentPath ++ List(a -> r))
                r.setValue(a, parent)
              case None =>
                throw new ORMException("The element <" + n.label + "> is empty.")
            }
          case m if (classOf[InverseAssociation[_, _]].isAssignableFrom(m.getReturnType)) =>
            val a = m.invoke(r).asInstanceOf[InverseAssociation[R, R]].association
            foreigns ++= n.child.filter(_.isInstanceOf[Elem]).map(n => (a -> n))
        }
      } catch {
        case e: NoSuchMethodException =>
          ormLog.warn("Could not process '" + n.label + "' of " + r.getClass)
      }
      case _ =>
    }
    // Now the record is ready to be saved.
    if (validate) r.save() else r.save_!()
    // Finally, process the foreigners.
    foreigns.foreach(p =>
      processNode(p._2, parentPath ++ List(p._1.asInstanceOf[Association[R, R]] -> r)))
    // And return our record.
    return r
  }

  protected def pickClass(node: Node): Class[_] = {
    var p = ""
    if (prefix != "") p = prefix + "."
    return Class.forName(p + node.label, true, Thread.currentThread().getContextClassLoader())
  }

  protected def setRecordField[R <: Record[R]](r: R, k: String, v: String): Unit = try {
    val m = r.getClass.getMethod(k)
    if (classOf[Field[_]].isAssignableFrom(m.getReturnType)) {    // only scalar fields are accepted
      val field = m.invoke(r).asInstanceOf[Field[Any]]
      val value = convertValue(r, k, v)
      r.setValue(field, value)
    }
  } catch {
    case e: NoSuchMethodException => ormLog.warn("Could not process '" + k + "' of " + r.getClass)
  }

  protected def prepareCriteria[R <: Record[R]](r: R, n: Node): Criteria[R] = {
    val crit = r.relation.criteria
    n.attributes.foreach(a => {
      val k = a.key
      val v = convertValue(r, k, a.value.toString)
      val field = r.getClass.getMethod(k).invoke(r).asInstanceOf[Field[Any]]
      crit.add(field EQ v)
    })
    return crit
  }

  protected def convertValue(r: Record[_], k: String, v: String): Any = try {
    r.getClass.getMethod(k).invoke(r).asInstanceOf[XmlSerializable[Any]].from(v)
  } catch {
    case _ => v
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
    return new Deployment(id, prefix, onExist, n.child.filter(n => n.isInstanceOf[Elem]), validate)
  } else throw new ORMException("<deployment> expected, but <" + n.label + "> found.")

  def readAll(n: Node): Seq[Deployment] = if (n.label == "deployments")
    (n \ "deployment").map(n => readOne(n))
  else throw new ORMException("<deployments> expected, but " + n.label + " found.")

}

class FileDeploymentHelper(f: File) {
  def process(): Unit = Deployment.readAll(XML.loadFile(f)).foreach(_.process)
}
