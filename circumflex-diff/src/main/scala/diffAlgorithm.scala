package ru.circumflex
package diff

import collection.JavaConversions._
import collection.mutable.{Buffer, ListBuffer}


trait DiffAlgorithm[T] {
  def diff(original: Seq[T],revised: Seq[T]): Patch[T]
}

class Patch[T] {
  private val deltas = ListBuffer[Delta[T]]()

  def applyTo(target: Seq[T]): Buffer[T] = {
    val res = ListBuffer[T]()
    val it = getDeltas.toList.listIterator(deltas.size)
    while (it.hasPrevious) {
      val delta = it.previous()
      delta.applyTo(res)
    }
    res
  }

  def restore(target: Seq[T]) = {
    val res = ListBuffer[T]()
    val it = getDeltas.toList.listIterator(deltas.size)
    while (it.hasPrevious) {
      val delta = it.previous()
      delta.restore(res)
    }
    res
  }

  def addDelta(delta: Delta[T]) {
    deltas += delta
  }

  def getDeltas = {
    deltas.sorted(DeltaComparator)
  }

}