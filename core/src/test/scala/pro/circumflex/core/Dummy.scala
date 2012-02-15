package pro.circumflex
package core

import java.util.UUID

/*! Simple dummy objects used by instantiation tests. */
class Dummy {
  val uuid = UUID.randomUUID.toString
  override def equals(obj: Any): Boolean = obj match {
    case d: Dummy => this.uuid.equals(d.uuid)
    case _ => false
  }
  override def hashCode: Int = uuid.hashCode
  override def toString = "Simple dummy instance"
}

object Dummy extends Dummy {
  override def toString = "Dummy singleton"
}

