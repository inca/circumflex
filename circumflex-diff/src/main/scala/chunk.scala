package ru.circumflex
package diff

class Chunk[T](val position: Int, val elements: Seq[T]) {

  def size = elements.size

  def verify(target: Seq[T]) {
    if (position + size - 1 > target.size) {
      throw new PatchFailedException("Incorrect Chunk: the position of chunk > target size")
    }
    var i = 0
    while (i < size)
      if (!(target(position + i) == elements(i)))
        throw new PatchFailedException("Incorrect Chunk: the chunk content doesn't match the target")
  }

  override def toString = "[position: " + position + ", size: " + size + ", elements: " + elements.mkString(", ") + "]"

}