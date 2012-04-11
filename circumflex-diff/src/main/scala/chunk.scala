package ru.circumflex
package diff

import collection.mutable.ListBuffer

class Chunk[T](private val _pos: Int = 0) {
  private val _elems = ListBuffer[T]()

  def this(position: Int, elements: Seq[T]) = {
    this(position)
    _elems.append(elements: _ *)
  }

  def position = _pos

  def elements = _elems
  def setElements(elements: Seq[T]) {
    _elems.clear()
    _elems.append(elements: _ *)
  }

  def size = _elems.size
  def last = position + size - 1

  def verify(target: Seq[T]) {
    if (last > target.size) {
      throw new PatchFailedException("Incorrect Chunk: the position of chunk > target size")
    }
    var i = 0
    while (i < size) {
      if (!(target(_pos + i) == _elems(i))) {
        throw new PatchFailedException("Incorrect Chunk: the chunk content doesn't match the target")
      }
    }
  }

  override def toString = "[position: " + position + ", size: " + size + ", elements: " + _elems + "]";
}