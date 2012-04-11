package ru.circumflex
package diff

import collection.mutable.Buffer

abstract class Delta[T](var original: Chunk[T], var revised: Chunk[T]) {

  protected abstract class Kind
  object CHANGE extends Kind
  object DELETE extends Kind
  object INSERT extends Kind

  def verify(target: Seq[T])
  def applyTo(target: Buffer[T])
  def restore(target: Buffer[T])
  def kind: Kind
}

object DeltaComparator extends Ordering[Delta[_]] {
  def compare(a: Delta[_], b: Delta[_]): Int = {
    val posA = a.original.position
    val posB = b.original.position
    if (posA > posB)
      return 1
    else if (posA < posB)
      return -1
    0
  }
}

class ChangeDelta[T](original: Chunk[T], revised: Chunk[T])
    extends Delta(original, revised) {

  def applyTo(target: Buffer[T]) {
    verify(target)
    val pos = original.position
    val size = original.size
    target.remove(pos, size)
    target.insert(pos, revised.elements: _ *)
  }

  def verify(target: Seq[T]) {
    original.verify(target)
    if (original.position > target.size)
      throw new PatchFailedException("Incorrect patch for delta: delta original position > target size")
  }

  def restore(target: Buffer[T]) {
    val pos = revised.position
    val size = revised.size
    target.remove(pos, size)
    target.insert(pos, original.elements: _ *)
  }

  override def toString = {
    "[ChangeDelta, position: " + original.position + ", elements: " +
        original.elements.mkString(", ") + " to " + revised.elements.mkString(", ") + "]";
  }

  def kind = CHANGE
}

class DeleteDelta[T](original: Chunk[T], revised: Chunk[T])
    extends Delta(original, revised) {

  def applyTo(target: Buffer[T]) {
    verify(target)
    target.remove(original.position, original.size)
  }

  def verify(target: Seq[T]) {
    original.verify(target)
  }

  def restore(target: Buffer[T]) {
    target.insert(revised.position, original.elements: _ *)
  }

  override def toString = {
    "[DeleteDelta, position: " + original.position + ", elements: " +
        original.elements.mkString(", ") + "]";
  }

  def kind = DELETE
}

class InsertDelta[T](original: Chunk[T], revised: Chunk[T])
    extends Delta(original, revised) {

  def applyTo(target: Buffer[T]) {
    verify(target)
    target.insert(original.position, revised.elements: _ *)
  }

  def verify(target: Seq[T]) {
    if (original.position > target.size)
      throw new PatchFailedException("Incorrect patch for delta: delta original position > target size")
  }

  def restore(target: Buffer[T]) {
    target.remove(revised.position, revised.size)
  }

  override def toString = {
    "[InsertDelta, position: " + original.position + ", elements: " + revised.elements.mkString(", ") + "]";
  }

  def kind = INSERT
}