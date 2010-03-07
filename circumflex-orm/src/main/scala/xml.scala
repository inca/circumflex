/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

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
    val r = cl.newInstance.asInstanceOf[Record[Any]]
    // process fields that were specified via attributes
    node.attributes.foreach(a => setRecordField(r, a.key, a.value.toString))
    // search for a record using provided attributes as criteria
    // nodes that have no children are forced not to be recreated
    findSimilar(r) match {
      case None if (node.child.size == 0) =>
        throw new ORMException("Could not find a record: " + node)
      case Some(rec) if (onExist == Deployment.Skip || node.child.size == 0) =>
        return rec
      case Some(rec) if (onExist == Deployment.Recreate) =>
        deleteSimilar(r)
      case Some(rec) if (onExist == Deployment.Update) =>
        r = rec.asInstanceOf[Record[Any]]
      case _ =>
    }
    // if we are still here, let's process the record further:
    // set up parents
    parentPath.foreach(p =>
      r.setField(p._1.localColumn.asInstanceOf[Column[Any, Any]], p._2.asInstanceOf[Record[_]].primaryKey))
    // foreigns will be processed after this record is saved
    var foreigns: Seq[Pair[Association[_, _], Node]] = Nil
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
      // convert a value of XmlSerializableColumn
      val value = field match {
        case f: ColumnField[_, _] if (f.column.isInstanceOf[XmlSerializableColumn[_]]) =>
          f.column.asInstanceOf[XmlSerializableColumn[_]].stringToValue(v)
        case _ => v
      }
      // set the field
      field.set(value)
    }
  } catch {
    case e: NoSuchMethodException => log.warn("Could not process '" + k + "' of " + r.getClass)
  }

  protected def prepareCriteria[R](r: Record[R]): Criteria[R] = {
    val crit = r.relation.criteria
    r.fieldsMap.keys.foreach(k =>
      crit.add(_.projection(k) eq r.fieldsMap.get(k).getOrElse(null)))
    return crit
  }

  protected def findSimilar[R](r: Record[R]): Option[Record[R]] =
    prepareCriteria(r).unique.asInstanceOf[Option[Record[R]]]

  protected def deleteSimilar[R](r: Record[R]): Unit =
    prepareCriteria(r).delete

  override def toString = id match {
    case "" => "deployment@" + hashCode
    case _ => id
  }
}
