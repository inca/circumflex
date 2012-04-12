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

  @inline protected def _sync[A](actions: => A): A =
    _lock.synchronized(actions)

  def getOption[E <: T](key: String, default: => Option[E]): Option[E] =
    retrieve(key) match {
      case Some(e: E) if (e.isValid) => Some(e)
      case _ => _sync {
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
  
  def put[E <: T](key: String, value: E) {
    _sync {
      store(key, value)
    }
  }

  def evict(key: String) {
    _sync {
      delete(key)
    }
  }

  def invalidate() {
    _sync {
      clear()
    }
  }

  def evictByPrefix(prefix: String) {
    _sync {
      keys.foreach { k =>
        if (k.startsWith(prefix))
          delete(k)
      }
    }
  }
}

trait NoLock[T <: Cached] extends Cache[T] {
  override protected def _sync[A](actions: => A) = actions
}

class CacheCell[A](initializer: => A) {
  protected var _value: A = _
  protected var _valid: Boolean = false

  def reinit() {
    _value = initializer
    _valid = true
  }

  def invalidate() {
    _valid = false
  }

  def isValid = _value match {
    case null => false
    case v: Cached => _valid && v.isValid
    case _ => _valid
  }

  def get: A = {
    if (!isValid)
      synchronized {
        if (!isValid)
          reinit()
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