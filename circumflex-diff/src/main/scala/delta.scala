package ru.circumflex
package diff

import collection.mutable.Buffer

abstract class Delta(var original: Chunk, var revised: Chunk) {

  protected abstract class Kind
  object CHANGE extends Kind
  object DELETE extends Kind
  object INSERT extends Kind

  def verify(target: Seq[_])
  def applyTo(target: Buffer[Any])
  def restore(target: Buffer[Any])
  def kind: Kind
}

object DeltaComparator extends Ordering[Delta] {
  def compare(a: Delta, b: Delta): Int = {
    val posA = a.original.position
    val posB = b.original.position
    if (posA > posB)
      return 1
    else if (posA < posB)
      return -1
    0
  }
}

class ChangeDelta(original: Chunk, revised: Chunk)
    extends Delta(original, revised) {

  def applyTo(target: Buffer[Any]) {
    verify(target)
    val pos = original.position
    val size = original.size
    target.remove(pos, size)
    target.insert(pos, revised.elements)
  }

  def verify(target: Seq[_]) {
    original.verify(target)
    if (original.position > target.size)
      throw new PatchFailedException("Incorrect patch for delta: delta original position > target size")
  }

  def restore(target: Buffer[Any]) {
    val pos = revised.position
    val size = revised.size
    target.remove(pos, size)
    target.insert(pos, original.elements)
  }

  override def toString = {
    "[ChangeDelta, position: " + original.position + ", elements: " +
        original.elements.mkString(", ") + " to " + revised.elements.mkString(", ") + "]";
  }

  def kind = CHANGE
}



class DeleteDelta(original: Chunk, revised: Chunk)
    extends Delta(original, revised) {

  def applyTo(target: Buffer[Any]) {
    verify(target)
    target.remove(original.position, original.size)
  }

  def verify(target: Seq[_]) {
    original.verify(target)
  }

  def restore(target: Buffer[Any]) {
    target.insert(revised.position, original.elements)
  }

  override def toString = {
    "[DeleteDelta, position: " + original.position + ", elements: " +
        original.elements.mkString(", ") + "]";
  }

  def kind = DELETE
}

class InsertDelta(original: Chunk, revised: Chunk)
    extends Delta(original, revised) {

  def applyTo(target: Buffer[Any]) {
    verify(target)
    target.insert(original.position, revised.elements)
  }

  def verify(target: Seq[_]) {
    if (original.position > target.size)
      throw new PatchFailedException("Incorrect patch for delta: delta original position > target size")
  }

  def restore(target: Buffer[Any]) {
    target.remove(revised.position, revised.size)
  }

  override def toString = {
    "[InsertDelta, position: " + original.position + ", elements: " + original.elements.mkString(", ") + "]";
  }

  def kind = INSERT
}