package ru.circumflex.core

import collection.mutable.{HashMap, ListBuffer}

object GroupBy {

  def apply[K,V](it: Iterable[V], f: V => K): collection.Map[K, Seq[V]] =  {
    val result = new HashMap[K, ListBuffer[V]] {
      override def default(a: K) = new ListBuffer[V]
    }
    it.foreach(v => {
      val key = f(v)
      val buffer: ListBuffer[V] = result(key)
      buffer += v
      result += (key -> buffer)
    })
    result
  }

}