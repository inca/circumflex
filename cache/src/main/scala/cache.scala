package pro.savant.circumflex
package cache

import core._
import net.sf.ehcache.Element
import java.io.Serializable
import collection.mutable.HashMap
import java.util.Date
import collection.JavaConversions

/*! # Cache API

Circumflex offers a tiny API which simplifies caching arbitrary application data.

Circumflex Cache API essentially consists of two traits: `Cached` and `Cache`.

The `Cached` trait, as the name implies, is mixed into the objects you wish
to cache to provide them with following features:

  * the timestamp of instantiation is remembered inside the `_createdAt` field;
  *
  * the `expired` method can be used to specify the conditions under which
    the object retrieved from cache cannot be used anymore;

  * objects can be marked as invalid explicitly by invoking the `invalidate()`
    method;

  * objects can be marked "clean" again by calling the `reclaim()` method,
    which clears the invalidation flag and updates the instance creation time
    to current time.
*/
trait Cached extends Serializable {
  protected var _createdAt = new Date(System.currentTimeMillis)
  CACHE_LOG.trace("Instantiated new " + this.getClass.getSimpleName)

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

/* The `Cache` trait defines simple interface for storing and retrieving
`Cached` objects.

Accessing and storing objects is implemented with synchronization
allowing caches to be used in multi-threaded environments (e.g. web applications).

Implementation should only define storage-related methods:
`retrieve`, `store`, `delete`, `clear` and `keys`
(they all are protected) and should not deal with synchronization.

## Working with cache

Cache clients should use `getOption` or `get` for value retrieval. Both methods
use double-checked locking algorithm to lookup a value (therefore, the `retrieve`
 method can be invoked twice) and accept the by-name `default` argument which
is returned in case of cache miss (the cache in also updated with such value).

Objects are also checked for validity using the `isValid` method
(see `Cached`). In other words, Cache guarantees to return only
valid instances of `Cached`.

Following example illustrates common usage scenario:

``` {.scala}
class Data extends Cached {
  def expired = { ... }
}

object DataCache extends Cache[Data] {
  ...
}

DataCache.get("myData", {
  // Some costly initialization code,
  // which only runs in case of cache miss or
  // if retrieved object is invalid
})
```
*/
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
            case Some(e) if (e.isValid) =>
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
    _lock.synchronized {
      store(key, value)
    }
  }

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

/*! ## Cache with no locks

If you use cache within a single thread, you can omit the synchronization
and thus gain a significant performance boost by mixing in the `NoLock` trait:

``` {.scala}
object DataCache extends Cache[Data] with NoLock[Data] {
  ...
}
```
*/
trait NoLock[T <: Cached] extends Cache[T] {

  override def getOption[E <: T](key: String, default: => Option[E]): Option[E] =
    retrieve(key) match {
      case Some(e: E) if (e.isValid) => Some(e)
      case _ =>
        default match {
          case Some(e) if (e.isValid) =>
            store(key, e)
            Some(e)
          case _ => None
        }
    }

  override def put[E <: T](key: String, value: E) {
    store(key, value)
  }

  override def evict(key: String) {
    delete(key)
  }

  override def invalidate() {
    clear()
  }

  override def evictByPrefix(prefix: String) {
    keys.foreach { k =>
      if (k.startsWith(prefix))
        delete(k)
    }
  }
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

/*! ## Cache implementations

There are several cache storage implementations available for you to use:

  * `Ehcache` is backed by industrial-grade Terracotta Ehcache library,
    which provides tons of configuration possibilities;

  * `HashCache` is backed by mutable `HashMap`, it should be used for
    testing purposes only. We recommend you to switch to `Ehcache` in production.

  * `ContextCache` uses Circumflex Context as a cache storage. Since
    Circumflex Context is bound to thread-local context, redundant
    synchronization is omitted there.

Ehcache uses `ehcacheManager` method of package object
`pro.savant.circumflex.cache` to obtain the `CacheManager` instance.
Set the `cx.ehcacheManager` configuration parameter to override the default
cache manager.

*/
class Ehcache[T <: Cached](val name: String) extends Cache[T] {

  val ehcache = ehcacheManager.addCacheIfAbsent(name)

  protected def retrieve(key: String): Option[T] = {
    val e = ehcache.get(key)
    if (e == null) None
    else Some(e.getObjectValue.asInstanceOf[T])
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

class ContextCache[T <: Cached](val key: String)
    extends HashCache[T]
    with NoLock[T] {

  override def storage: HashMap[String, T] = ctx.get(key) match {
    case Some(m: HashMap[String, T]) => m
    case _ =>
      val m = new HashMap[String, T]
      ctx += (key -> m)
      m
  }

}
