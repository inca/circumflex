package pro.savant.circumflex
package diff

sealed trait Chunk[T]{
  def original: Seq[T]
  def revised: Seq[T]
}

class InsertChunk[T](val original: Seq[T], val revised: Seq[T])
    extends Chunk[T] {

  override def toString = "[InsertChunk, elements: " +
      revised.mkString(", ") + "]"
}

class DeleteChunk[T](val original: Seq[T], val revised: Seq[T])
    extends Chunk[T] {

  override def toString = "[DeleteChunk, elements: " +
      original.mkString(", ") + "]"
}

class ChangeChunk[T](val original: Seq[T], val revised: Seq[T])
    extends Chunk[T] {

  override def toString = "[ChangeChunk, elements: " +
      original.mkString(", ") + " to " + revised.mkString(", ") + "]"
}

class EqualChunk[T](val original: Seq[T], val revised: Seq[T])
    extends Chunk[T] {
  override def toString = "[EqualChunk, elements: " +
      original.mkString(", ") + "]"
}

class Difference[T](val chunks: Seq[Chunk[T]]) extends Seq[Chunk[T]] {
  def length = chunks.size
  def apply(idx: Int) = chunks(idx)
  def iterator = chunks.iterator
}