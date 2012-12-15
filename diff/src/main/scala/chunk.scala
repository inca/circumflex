package pro.savant.circumflex
package diff

/*! # Chunks

Diff results are reported as a sequence of chunks. Each chunk defines
a correspondence between two sequences: `original` and `revised`.

`Chunk` is a sealed trait with only four implementations:

  * `InsertChunk` designates that elements have been added
    (the `original` sequence is empty);

  * `DeleteChunk` designates that elements have been removed
    (the `revised` sequence is empty);

  * `ChangeChunk` designates that elements have been changed;

  * `EqualChunk` designates that elements have not been changed
    (the two sequences are equal).
*/
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