package pro.savant.circumflex
package cluster

import core._, xml._

class Prop(val name: String) extends TextHolder {

  def elemName = name

}

class PropsHolder(val elemName: String)
    extends ListHolder[Prop] {

  def read = {
    case p => new Prop(p)
  }

  def toMap = Map(children.map(p => p.name -> p.getOrElse("")): _*)

}