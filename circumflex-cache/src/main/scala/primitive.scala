package ru.circumflex
package cache

import java.util.Date
import collection.{Map, Set}

trait CachedPrimitive[T] extends Cached {
  def expired = false
  def value: T
}

class CachedString(val value: String) extends CachedPrimitive[String]
class CachedInt(val value: Int) extends CachedPrimitive[Int]
class CachedLong(val value: Long) extends CachedPrimitive[Long]
class CachedBoolean(val value: Boolean) extends CachedPrimitive[Boolean]
class CachedFloat(val value: Float) extends CachedPrimitive[Float]
class CachedDouble(val value: Double) extends CachedPrimitive[Double]
class CachedBigDecimal(val value: BigDecimal) extends CachedPrimitive[BigDecimal]
class CachedDate(val value: Date) extends CachedPrimitive[Date]
class CachedArray[T](val value: Array[T]) extends CachedPrimitive[Array[T]]
class CachedSeq[T](val value: Seq[T]) extends CachedPrimitive[Seq[T]]
class CachedSet[T](val value: Set[T]) extends CachedPrimitive[Set[T]]
class CachedMap[K, V](val value: Map[K, V]) extends CachedPrimitive[Map[K, V]]
class CachedTuple2[T1, T2](val value: (T1, T2)) extends CachedPrimitive[(T1, T2)]
class CachedTuple3[T1, T2, T3](val value: (T1, T2, T3)) extends CachedPrimitive[(T1, T2, T3)]