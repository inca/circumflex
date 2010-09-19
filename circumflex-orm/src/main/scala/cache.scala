package ru.circumflex.orm

import ru.circumflex.core._
import collection.mutable.HashMap
import java.util.concurrent.ConcurrentHashMap

/*!# Context-Level Cache

The `CacheService` trait defines minimum functionality required for organizing
context-level cache.

The context-level cache is designed to maintain records within a single
transaction. This functionality is required for all data-retrieval operations.

The cache consists of two logical parts:

  1. *record cache* holds individual records by their relations and `id`s;
  2. *inverse cache* holds sequences of records by their associations and their parent's `id`s.
*/
trait CacheService {

  /**
   * Clears the whole cache.
   */
  def invalidate: Unit = {
    invalidateRecords
    invalidateInverse
  }

  /*!## Records Cache

  Following methods are used to maintain records cache:

  * `invalidateRecords` clears all records from cache or only those who correspond
  to specified `relation`;
  * `getRecord` retrieves a record from cache by specified `relation` and `id`;
  * `updateRecord` updates a cache with specified `record`;
  * `evictRecord` removes a record from cache by specified `relation` and `id`.
  */
  def invalidateRecords: Unit
  def invalidateRecords[PK, R <: Record[PK, R]](
      relation: Relation[PK, R]): Unit
  def cacheRecord[PK, R <: Record[PK, R]](
      id: PK, relation: Relation[PK, R], record: => Option[R]): Option[R]
  def evictRecord[PK, R <: Record[PK, R]](
      id: PK, relation: Relation[PK, R]): Unit
  def updateRecord[PK, R <: Record[PK, R]](
      id: PK, relation: Relation[PK, R], record: R): R = {
    evictRecord(id, relation)
    cacheRecord(id, relation, Some(record))
    record
  }

  /*!## Inverse Cache

  Following methods are used to maintain inverse cache:

  * `invalidateInverse` clears all records from inverse cache or only those who
  correspond to specified `association`;
  * `cacheInverse` retrieves children records from cache by specified `association`
  and their `parentId` or updates cache correspondingly;
  * `updateInverse` updates an inverse cache with specified `children`;
  * `evictRecord` removes children from inverse cache by specified `association` and `parentId`.
  */
  def invalidateInverse: Unit
  def invalidateInverse[K, C <: Record[_, C], P <: Record[K, P]](
      association: Association[K, C, P]): Unit
  def cacheInverse[K, C <: Record[_, C], P <: Record[K, P]](
      parentId: K, association: Association[K, C, P], children: => Seq[C]): Seq[C]
  def evictInverse[K, C <: Record[_, C], P <: Record[K, P]](
      parentId: K, association: Association[K, C, P]): Unit
  def updateInverse[K, C <: Record[_, C], P <: Record[K, P]](
      parentId: K, association: Association[K, C, P], children: Seq[C]): Seq[C] = {
    evictInverse(parentId, association)
    cacheInverse(parentId, association, children)
  }

}

/*! The default cache service implementation relies on Scala mutable `HashMap`s.
It can be overriden by setting the `orm.cacheService` parameter. */
class DefaultCacheService extends CacheService {

  class CacheMap extends HashMap[Any, HashMap[Any, Any]] {
    override def apply(key: Any): HashMap[Any, Any] =
      super.getOrElseUpdate(key, new HashMap[Any, Any])
  }

  protected val _recordsCache = new CacheMap
  protected val _inverseCache = new CacheMap

  // Records cache

  def invalidateRecords: Unit = {
    _recordsCache.clear()
    Cacheable.relations.foreach(_.invalidateCache)
  }
  def invalidateRecords[PK, R <: Record[PK, R]](relation: Relation[PK, R]): Unit =
    relation match {
      case c: Cacheable[_, _] => c.invalidateCache
      case _ => _recordsCache.remove(relation)
    }
  def evictRecord[PK, R <: Record[PK, R]](id: PK, relation: Relation[PK, R]): Unit =
    relation match {
      case c: Cacheable[_, _] => c.evict(id)
      case _ => _recordsCache(relation).remove(id)
    }
  def cacheRecord[PK, R <: Record[PK, R]](
      id: PK, relation: Relation[PK, R], record: => Option[R]): Option[R] =
    relation match {
      case c: Cacheable[PK, R] => c.cache(id, record)
      case _ =>
        val c = _recordsCache(relation)
        c.get(id).map(_.asInstanceOf[R]) orElse {
          val v = record
          v.map { r =>
            c.update(id, r)
            r
          }
        }
    }

  // Inverse cache

  def invalidateInverse: Unit =
    _inverseCache.clear()
  def invalidateInverse[K, C <: Record[_, C], P <: Record[K, P]](association: Association[K, C, P]): Unit =
    _inverseCache(association).clear()
  def evictInverse[K, C <: Record[_, C], P <: Record[K, P]](
      parentId: K, association: Association[K, C, P]): Unit =
    _inverseCache(association).remove(parentId)
  def cacheInverse[K, C <: Record[_, C], P <: Record[K, P]](
      parentId: K, association: Association[K, C, P], children: => Seq[C]): Seq[C] =
    _inverseCache(association).getOrElseUpdate(parentId, children).asInstanceOf[Seq[C]]

}

/*! The `CacheService` object is used to retrieve context-bound cache service. */
object CacheService {
  def get: CacheService = ctx.get("orm.cacheService") match {
    case Some(cs: CacheService) => cs
    case _ =>
      val cs = cx.instantiate[CacheService]("orm.cacheService", new DefaultCacheService)
      ctx.update("orm.cacheService", cs)
      return cs
  }
}

/*!# Application-Level Cache

Circumflex ORM lets you organize application-scope cache for any relation of your
application: just mix in the `Cacheable` trait into your relation. Note that since
one record instance may become accessible to several threads, the modification
of such records is subject for concurrency control.
*/
trait Cacheable[PK, R <: Record[PK, R]] extends Relation[PK, R] { this: R =>
  protected val _cache = new ConcurrentHashMap[PK, R]

  def cache(id: PK, record: => Option[R]): Option[R] =
    if (_cache.containsKey(id))
      return Some(_cache.get(id))
    else {
      val v = record
      v.map { r =>
        _cache.put(id, r)
        r
      }
    }
  def evict(id: PK): Unit =
    _cache.remove(id)
  def invalidateCache(): Unit =
    _cache.clear()
  def contains_?(id: PK): Boolean =
    _cache.contains(id)

  afterInsert(r => cache(r.PRIMARY_KEY(), r))
  afterUpdate(r => cache(r.PRIMARY_KEY(), r))
  afterDelete(r => evict(r.PRIMARY_KEY()))

  Cacheable.add(this)
}

object Cacheable {
  private var _relations: Seq[Cacheable[_, _]] = Nil
  def relations = _relations
  def add[PK, R <: Record[PK, R]](relation: Cacheable[PK, R]): this.type = {
    _relations ++= List(relation)
    return this
  }
}