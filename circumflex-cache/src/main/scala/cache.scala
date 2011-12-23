package ru.circumflex
package cache

import core._
import net.sf.ehcache.Element
import java.io.Serializable
import collection.mutable.HashMap
import java.util.Date
import collection.JavaConversions

trait Cache[T <: Cached] {
  private val _lock = new Object

  protected def retrieve(key: String): Option[T]
  protected def store(key: String, value: T)
  protected def delete(key: String)
  protected def clear()
  def keys: Iterable[String]

  def getOption[E <: T](key: String, default: => Option[E]): Option[E] =
    retrieve(key) match {
      case Some(e: E) if (e.isValid) => Some(e)
      case _ => _lock.synchronized {
        retrieve(key) match {
          case Some(e: E) if (e.isValid) => Some(e)
          case _ => default match {
            case Some(e) =>
              store(key, e)
              Some(e)
            case _ => None
          }
        }
      }
    }

  def get[E <: T](key: String, default: => E): E =
    getOption(key, Some(default)).get

  def evict(key: String) {
    _lock.synchronized {
      delete(key)
    }
  }

  def invalidate() {
    _lock.synchronized {
      clear()
    }
  }

  def evictByPrefix(prefix: String) {
    _lock.synchronized {
      keys.foreach { k =>
        if (k.startsWith(prefix))
          delete(k)
      }
    }
  }
}

class CacheCell[A <: Cached](val initializer: () => A) {
  protected var _value: A = _
  def get: A = {
    if (_value == null || !_value.isValid)
      synchronized {
        if (_value == null || !_value.isValid)
          _value = initializer()
      }
    _value
  }
}

class Ehcache[T <: Cached](val name: String) extends Cache[T] {
  val ehcache = ehcacheManager.addCacheIfAbsent(name)
  protected def retrieve(key: String): Option[T] = {
    val e = ehcache.get(key)
    if (e == null) None
    else Some(e.getValue.asInstanceOf[T])
  }
  protected def store(key: String, value: T) {
    val e = new Element(key, value)
    ehcache.put(e)
  }
  protected def delete(key: String) {
    ehcache.remove(key)
  }
  protected def clear() {
    ehcache.removeAll()
  }
  def keys: Iterable[String] = JavaConversions
      .collectionAsScalaIterable(ehcache.getKeys)
      .asInstanceOf[Iterable[String]]
}

class HashCache[T <: Cached] extends Cache[T] {
  protected val _storage = new HashMap[String, T]
  def storage: HashMap[String, T] = _storage
  protected def retrieve(key: String) = storage.get(key)
  protected def store(key: String, value: T) {
    storage.update(key, value)
  }
  protected def delete(key: String) {
    storage.remove(key)
  }
  protected def clear() {
    _storage.clear()
  }
  def keys: Iterable[String] = _storage.keys
}

class ContextCache[T <: Cached](val key: String) extends HashCache[T] {
  override def storage: HashMap[String, T] = ctx.get(key) match {
    case Some(m: HashMap[String, T]) => m
    case _ =>
      val m = new HashMap[String, T]
      ctx += (key -> m)
      m
  }
}

trait Cached extends Serializable {
  protected var _createdAt = new Date(System.currentTimeMillis)
  CACHE_LOG.debug("Instantiated new " + this.getClass.getSimpleName)
  protected var _invalidated = false
  def expired: Boolean
  def createdAt = _createdAt
  def isValid = !_invalidated && !expired
  def invalidate() {
    _invalidated = true
  }
  def reclaim() {
    _createdAt = new Date(System.currentTimeMillis)
    _invalidated = false
  }
}