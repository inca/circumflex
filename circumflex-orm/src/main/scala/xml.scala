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
import javax.xml.stream.XMLStreamReader
import xml.{Node, Elem, NodeSeq}
import org.slf4j.LoggerFactory

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
  object Keep extends OnExistAction
  object Recreate extends OnExistAction

  def readOne(n: Node): Deployment = if (n.label == "deployment") {
    val id = (n \ "@id").text
    val prefix = (n \ "@prefix").text
    val onExist = (n \ "@onExist").text match {
      case "keep" => Deployment.Keep
      case "recreate" => Deployment.Recreate
      case _ => Deployment.Keep
    }
    return new Deployment(id, prefix, onExist, n.child.filter(n => n.isInstanceOf[Elem]))
  } else throw new ORMException("<deployment> expected, but <" + n.label + "> found.")

  def readAll(n: Node): Seq[Deployment] = if (n.label == "deployments")
    (n \ "deployment").map(n => readOne(n))
  else throw new ORMException("<deployments> expected, but " + n.label + " found.")

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

  def process(): Unit = entries.foreach(e => processNode(e, Map()))

  protected def processNode(node: Node, parentPath: Map[Association[_, _], Record[_]]): Unit = {
    val cl = pickClass(node)
    val r = cl.newInstance.asInstanceOf[Record[_]]
    // process fields that were specified via attributes
    node.attributes.foreach(a => setRecordField(r, a.key, a.value.toString))
    // search for a record using provided attributes as criteria
    println(r)
  }

  protected def pickClass(node: Node): Class[_] = {
    var p = ""
    if (prefix != "") p = prefix + "."
    return Class.forName(p + node.label)
  }

  protected def setRecordField(r: Record[_], k: String, v: String): Unit = try {
    // try to find a specified column by intospecting record's methods
    val m = r.getClass.getMethod(k)
    if (classOf[Field[_]].isAssignableFrom(m.getReturnType)) {
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
    case _ => log.warn("Could not set the field " + k + " on record of " + r.getClass)
  }

  override def toString = id match {
    case "" => "deployment@" + hashCode
    case _ => id
  }
}
