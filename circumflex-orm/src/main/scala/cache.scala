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
  def invalidateRecords[R <: Record[R]](relation: Relation[R]): Unit
  def getRecord[R <: Record[R]](relation: Relation[R], id: Any): Option[R]
  def updateRecord[R <: Record[R]](record: R): Unit
  def evictRecord[R <: Record[R]](relation: Relation[R], id: Any): Unit

  /*!## Inverse Cache

  Following methods are used to maintain inverse cache:

  * `invalidateInverse` clears all records from inverse cache or only those who
  correspond to specified `association`;
  * `getInverse` retrieves children records from cache by specified `inverse` association
  and their `parentId`;
  * `updateInverse` updates an inverse cache with specified `children`;
  * `evictRecord` removes children from inverse cache by specified `association` and `parentId`.
  */
  def invalidateInverse: Unit
  def invalidateInverse[P <: Record[P], C <: Record[C]](association: Association[C, P]): Unit
  def getInverse[P <: Record[P], C <: Record[C]](association: Association[C, P],
                                                 parentId: Any): Option[Seq[C]]
  def updateInverse[P <: Record[P], C <: Record[C]](association: Association[C, P],
                                                    parentId: Any,
                                                    children: Seq[C]): Unit
  def evictInverse[P <: Record[P], C <: Record[C]](association: Association[C, P],
                                                   parentId: Any): Unit

}

class HashMapCacheService extends CacheService {

  class CacheMap extends HashMap[Any, HashMap[Any, Any]] {
    override def get(key: Any): Option[HashMap[Any, Any]] = super.get(key) orElse {
      val m = new HashMap[Any, Any]
      update(key, m)
      m
    }
  }

  protected val _recordsCache = new CacheMap
  protected val _inverseCache = new CacheMap

  def invalidateRecords: Unit =
    _recordsCache.clear
  def invalidateRecords[R <: Record[R]](relation: Relation[R]): Unit =
    _recordsCache.remove(relation)
  def getRecord[R <: Record[R]](relation: Relation[R], id: Any): Option[R] =
    relation match {
      case c: Cacheable[R] => c.getCachedRecord(id)
      case _ => _recordsCache(relation).get(id).asInstanceOf[Option[R]]
    }
  def updateRecord[R <: Record[R]](record: R): Unit =
    if (!record.transient_?) record.relation match {
      case c: Cacheable[R] => c.updateRecordCache(record)
      case _ => _recordsCache(record.relation).update(record.id(), record)
    }
  def evictRecord[R <: Record[R]](relation: Relation[R], id: Any): Unit =
    relation match {
      case c: Cacheable[R] => c.evictRecordCache(id)
      case _ => _recordsCache(relation).remove(id)
    }


  def invalidateInverse: Unit =
    _inverseCache.clear
  def invalidateInverse[P <: Record[P], C <: Record[C]](association: Association[C, P]): Unit =
    _inverseCache.remove(association)
  def getInverse[P <: Record[P], C <: Record[C]](association: Association[C, P],
                                                 parentId: Any): Option[Seq[C]] =
    _inverseCache(association).get(parentId).asInstanceOf[Option[Seq[C]]]
  def updateInverse[P <: Record[P], C <: Record[C]](association: Association[C, P],
                                                    parentId: Any,
                                                    children: Seq[C]): Unit =
    _inverseCache(association).update(parentId, children)
  def evictInverse[P <: Record[P], C <: Record[C]](association: Association[C, P],
                                                   parentId: Any): Unit =
    _inverseCache(association).remove(parentId)

}

/*! The default cache service implementation relies on Scala mutable `HashMap`s.
It can be overriden by setting the `orm.cacheService` parameter. */

object DefaultCacheService extends HashMapCacheService

/*! The `CacheService` object is used to retrieve context-bound cache service. */

object CacheService {
  def get: CacheService = ctx.get("orm.cacheService") match {
    case Some(cs: CacheService) => cs
    case _ =>
      val cs = cx.instantiate[CacheService]("orm.cacheService", DefaultCacheService)
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
trait Cacheable[R <: Record[R]] extends Relation[R] {
  protected var _cache = new ConcurrentHashMap[Any, R]

  def updateRecordCache(record: R): R = {
    _cache.put(record.id(), record)
    return record
  }
  def getCachedRecord(id: Any): Option[R] = _cache.get(id)
  def evictRecordCache(id: Any): Unit = _cache.remove(id)

  /**
   * Clears applciation-scoped cache.
   */
  def invalidateCache(): Unit = {
    _cache = new ConcurrentHashMap[Any, R]
  }

  afterInsert(r => updateRecordCache(r))
  afterUpdate(r => updateRecordCache(r))
  afterDelete(r => evictRecordCache(r.id()))

}