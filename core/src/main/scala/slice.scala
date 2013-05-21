package pro.savant.circumflex
package core

import collection.mutable.{ListBuffer, Builder}
import collection.generic.CanBuildFrom
import collection.SeqLike
import java.io.Serializable

/*! # Slices API

Circumflex Slices API provide a couple of methods for projecting a Scala `Seq[V]`
onto a `Slice[K, V]` -- simple wrapper which associates a portion of this sequence
with specific key of type `K`.

Slice has all the methods you would expect in `Seq[V]` which generally return
new slice with modified view of underlying sequence.
*/

object Slice {

  @inline implicit def canBuildFrom[K, V, B] =
    new CanBuildFrom[Slice[K, V], B, Slice[K, B]] {

      def apply(from: Slice[K, V]) = new Builder[B, Slice[K, B]] {

        val buffer = new ListBuffer[B]

        def +=(elem: B) = {
          buffer += elem
          this
        }

        def clear() {
          buffer.clear()
        }

        def result() = new Slice[K, B](from.key, buffer.toSeq)
      }

      def apply() = throw new UnsupportedOperationException(
        "Cannot build slice from scratch.")
    }

}

class Slice[K, +V](val key: K, val elems: Seq[V])
    extends SeqLike[V, Slice[K, V]]
    with Serializable { self =>

  protected[this] def newBuilder = new Builder[V, Slice[K, V]] {

    val buffer = new ListBuffer[V]

    def +=(elem: V) = {
      buffer += elem
      this
    }

    def clear() {
      buffer.clear()
    }

    def result() = new Slice[K, V](self.key, buffer.toSeq)

  }

  def seq = elems

  def length = elems.size

  def apply(idx: Int) = elems.apply(idx)

  def iterator = elems.iterator

  override def toString = key + " -> {" +
      elems.map(_.toString).mkString(", ") + "}"

}

/*! Object `slices` contains following methods: */
object slices {

  /*!
  * `cut` performs initial sequence grouping (like `groupBy`),
    but preserves natural ordering of specified sequence.
    Therefore, the returned sequence can contain slices with same keys.

    In other words, it collapses adjancent elements (key-wise):

    ```
    [A, B, A, A, B, C] -> [A], [B], [A, A], [B], [C]
    ```
  */
  def cut[K, V](seq: Seq[V])(func: V => K): Seq[Slice[K, V]] = {
    val buffer = new ListBuffer[V]
    val result = new ListBuffer[Slice[K, V]]
    var k: Any = null
    seq.foreach { elem =>
      val key = func(elem)
      if (k == key) { // Append to existing buffer
        buffer += elem
      } else {
        // Flush old buffer
        if (buffer.size > 0)
          result += new Slice(k.asInstanceOf[K], buffer.toSeq)
        // Start new buffer
        buffer.clear()
        buffer += elem
        k = key
      }
    }
    // Flush last buffer
    if (buffer.size > 0)
      result += new Slice(k.asInstanceOf[K], buffer.toSeq)
    result.toSeq
  }

  /*!
  * `group` performs initial sequence grouping (using `groupBy`),
    ensuring that keys are unique across returning slices.

    In other words, it collapses all elements (key-wise):

    ```
    [A, B, A, A, B, C] -> [A, A, A], [B, B], [C]
    ```
  */
  def group[K, V](seq: Seq[V])(func: V => K): Seq[Slice[K, V]] =
    seq.groupBy(func).map(p => new Slice(p._1, p._2)).toSeq

}
