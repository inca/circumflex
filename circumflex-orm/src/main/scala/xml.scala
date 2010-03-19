package ru.circumflex.orm

import ORM._
import org.slf4j.LoggerFactory
import java.io.File
import xml.{XML, Node, Elem}

/**
 * Specifies that certain column values can be serialized to and deserialized
 * from XML representation.
 */
trait XmlSerializableColumn[T] {
  def stringToValue(str: String): T
  def valueToString(value: T): String = value.toString
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
    return new Deployment(id, prefix, onExist, n.child.filter(n => n.isInstanceOf[Elem]))
  } else throw new ORMException("<deployment> expected, but <" + n.label + "> found.")

  def readAll(n: Node): Seq[Deployment] = if (n.label == "deployments")
    (n \ "deployment").map(n => readOne(n))
  else throw new ORMException("<deployments> expected, but " + n.label + " found.")

}

class FileDeploymentHelper(f: File) {
  def process(): Unit = Deployment.readAll(XML.loadFile(f)).foreach(_.process)
}

/**
 * Deployment is a unit of work of XML import tool. It specifies the prefix
 * for record classes resolution, as well as the behavior, if certain records
 * already exist.
 */
class Deployment(val id: String,
                 val prefix: String,
                 val onExist: Deployment.OnExistAction,
                 val entries: Seq[Node]) {

  protected val log = LoggerFactory.getLogger("ru.circumflex.orm")

  def process(): Unit = try {
    entries.foreach(e => processNode(e, Nil))
    tx.commit
  } catch {
    case e =>
      tx.rollback
      throw e
  }

  protected def processNode(node: Node, parentPath: Seq[Pair[Association[_, _], Record[_]]]): Record[_] = {
    val cl = pickClass(node)
    var r = cl.newInstance.asInstanceOf[Record[Any]]
    // nodes that have no children are forced not to be recreated
    if (node.attributes.next != null) {
      val crit = prepareCriteria(r, node)
      crit.unique match {
        case None if (node.child.size == 0) =>
          throw new ORMException("Could not find a record: " + node)
        case Some(rec: Record[_]) if (onExist == Deployment.Skip || node.child.size == 0) =>
          return rec
        case Some(rec: Record[_]) if (onExist == Deployment.Recreate) =>
          crit.delete()
        case Some(rec: Record[_]) if (onExist == Deployment.Update) =>
          r = rec.asInstanceOf[Record[Any]]
        case _ =>
      }
    }
    // if we are still here, let's process the record further:
    // set up parents
    parentPath.foreach(p =>
      r.setField(p._1.localColumn.asInstanceOf[Column[Any, Any]], p._2.asInstanceOf[Record[_]].primaryKey))
    // foreigns will be processed after this record is saved
    var foreigns: Seq[Pair[Association[_, _], Node]] = Nil
    // set up fields from attrbutes
    node.attributes.foreach(a => setRecordField(r, a.key, a.value.toString))
    // traverse children nodes to set fields or initialize associative stuff
    node.child.foreach {
      case n: Elem => try {
        r.getClass.getMethod(n.label) match {
          case m if (classOf[RecordScalar[_]].isAssignableFrom(m.getReturnType)) =>
            setRecordField(r, n.label, n.text.trim)
          case m if (classOf[RecordLocalAssociation[_, _]].isAssignableFrom(m.getReturnType)) =>
            n.child.find(_.isInstanceOf[Elem]) match {
              case Some(n) =>
                val a = m.invoke(r).asInstanceOf[RecordLocalAssociation[Any, Any]].association
                val parent = processNode(n, parentPath ++ List(a -> r))
                r.setField(a.localColumn.asInstanceOf[Column[Any, Any]],
                  parent.asInstanceOf[Record[_]].primaryKey)
              case None =>
                throw new ORMException("The element <" + n.label + "> is empty.")
            }
          case m if (classOf[RecordForeignAssociation[_, _]].isAssignableFrom(m.getReturnType)) =>
            val a = m.invoke(r).asInstanceOf[RecordForeignAssociation[Any, Any]].association
            foreigns ++= n.child.filter(_.isInstanceOf[Elem]).map(n => (a -> n))
        }
      } catch {
        case e: NoSuchMethodException => log.warn("Could not process '" + n.label + "' of " + r.getClass)
      }
      case _ =>
    }
    // now the record is ready to be saved
    r.save()
    // finally, process the foreigners
    foreigns.foreach(p => processNode(p._2, parentPath ++ List(p._1.asInstanceOf[Association[_, _]] -> r)))
    // and return our record
    return r
  }

  protected def pickClass(node: Node): Class[_] = {
    var p = ""
    if (prefix != "") p = prefix + "."
    return Class.forName(p + node.label, true, Thread.currentThread().getContextClassLoader())
  }

  protected def setRecordField(r: Record[_], k: String, v: String): Unit = try {
    // try to find a specified column by intospecting record's methods
    val m = r.getClass.getMethod(k)
    if (classOf[RecordScalar[_]].isAssignableFrom(m.getReturnType) &&
            classOf[Field[_]].isAssignableFrom(m.getReturnType)) {    // only scalar fields are accepted
      val field = m.invoke(r).asInstanceOf[Field[Any]]
      val value = convertValue(r, k, v)
      // set the field
      field.set(value)
    }
  } catch {
    case e: NoSuchMethodException => log.warn("Could not process '" + k + "' of " + r.getClass)
  }

  protected def prepareCriteria[R](r: Record[R], n: Node): Criteria[R] = {
    val crit = r.relation.criteria
     n.attributes.foreach(a => {
       val k = a.key
       val v = convertValue(r, k, a.value.toString)
       val col = r.getClass.getMethod(k).invoke(r)
            .asInstanceOf[ColumnField[Any, R]].column
       crit.add(_.projection(col) eq v)
     })
    return crit
  }

  // convert a value of XmlSerializableColumn
  protected def convertValue(r: Record[_], k: String, v: String): Any = try {
    r.getClass.getMethod(k).invoke(r)
            .asInstanceOf[ColumnField[Any, _]].column
            .asInstanceOf[XmlSerializableColumn[_]].stringToValue(v)
  } catch {
    case _ => v
  }

  override def toString = id match {
    case "" => "deployment@" + hashCode
    case _ => id
  }
}
