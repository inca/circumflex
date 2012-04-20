package ru.circumflex
package diff

class Chunk[T](val original: Seq[T], val revised: Seq[T])

class InsertChunk[T](original: Seq[T], revised: Seq[T])
    extends Chunk(original, revised) {

  override def toString = "[InsertChunk, elements: " +
      revised.mkString(", ") + "]";
}

class DeleteChunk[T](original: Seq[T], revised: Seq[T])
    extends Chunk(original, revised) {

  override def toString = "[DeleteChunk, elements: " +
      original.mkString(", ") + "]";
}

class ChangeChunk[T](original: Seq[T], revised: Seq[T])
    extends Chunk(original, revised) {

  override def toString = "[ChangeChunk, elements: " +
      original.mkString(", ") + " to " + revised.mkString(", ") + "]";
}

class EqualChunk[T](original: Seq[T], revised: Seq[T])
    extends Chunk(original, revised) {
  override def toString = "[EqualChunk, elements: " +
      original.mkString(", ") + "]";
}

class Difference[T](val chunks: Seq[Chunk[T]]) extends Seq[Chunk[T]] {
  def length = chunks.size
  def apply(idx: Int) = chunks(idx)
  def iterator = chunks.iterator
}