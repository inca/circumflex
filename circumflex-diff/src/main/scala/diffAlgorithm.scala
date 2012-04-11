package ru.circumflex
package diff

import collection.JavaConversions._
import collection.mutable.{Buffer, ListBuffer}


trait DiffAlgorithm {
  def diff(original: Seq[_],revised: Seq[_]): Patch
}

class Patch {
  private val deltas = ListBuffer[Delta]()

  def applyTo(target: Seq[_]): Buffer[_] = {
    val res = ListBuffer[Any]()
    val it = getDeltas.toList.listIterator(deltas.size)
    while (it.hasPrevious) {
      val delta = it.previous()
      delta.applyTo(res)
    }
    res
  }

  def restore(target: Seq[_]) = {
    val res = ListBuffer[Any]()
    val it = getDeltas.toList.listIterator(deltas.size)
    while (it.hasPrevious) {
      val delta = it.previous()
      delta.restore(res)
    }
    res
  }

  def addDelta(delta: Delta) {
    deltas += delta
  }

  def getDeltas = {
    deltas.sorted(DeltaComparator)
  }

}