package ru.circumflex.orm

import ru.circumflex.core._
import collection.mutable.HashMap

/*!# Context-Level Cache

The `CacheService` trait defines minimum functionality required for organizing
context-level cache.

The context-level cache is designed to maintain records within a single
transaction. This functionality is required for all data-retrieval operations.

The cache consists of two logical parts:

  1. *record cache* holds individual records by their relations and `id`s;
  2. *inverse cache* holds sequences of records by their inverse associations and their parent's `id`s.
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
  def getRecord[R <: Record[R]](relation: Relation[R], id: Long): Option[R]
  def updateRecord[R <: Record[R]](record: R): Unit
  def evictRecord[R <: Record[R]](relation: Relation[R], id: Long): Unit

  /*!## Inverse Cache

  Following methods are used to maintain inverse cache:

  * `invalidateInverse` clears all records from inverse cache or only those who
  correspond to specified `inverse` association;
  * `getInverse` retrieves children records from cache by specified `inverse` association
  and their `parentId`;
  * `updateInverse` updates an inverse cache with specified `children`;
  * `evictRecord` removes children from inverse cache by specified `inverse` and `parentId`.
  */
  def invalidateInverse: Unit
  def invalidateInverse[P <: Record[P], C <: Record[C]](inverse: InverseAssociation[P, C]): Unit
  def getInverse[P <: Record[P], C <: Record[C]](inverse: InverseAssociation[P, C],
                                                 parentId: Long): Option[Seq[C]]
  def updateInverse[P <: Record[P], C <: Record[C]](inverse: InverseAssociation[P, C],
                                                    parentId: Long,
                                                    children: Seq[C]): Unit
  def evictInverse[P <: Record[P], C <: Record[C]](inverse: InverseAssociation[P, C],
                                                   parentId: Long): Unit

}

class HashMapCacheService extends CacheService {

  class CacheMap extends HashMap[Any, HashMap[Long, Any]] {
    override def get(key: Any): Option[HashMap[Long, Any]] = super.get(key) orElse {
      val m = new HashMap[Long, Any]
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
  def getRecord[R <: Record[R]](relation: Relation[R], id: Long): Option[R] =
    _recordsCache(relation).get(id).asInstanceOf[Option[R]]
  def updateRecord[R <: Record[R]](record: R): Unit =
    _recordsCache(record.relation).update(record.id(), record)
  def evictRecord[R <: Record[R]](relation: Relation[R], id: Long): Unit =
    _recordsCache(relation).remove(id)

  def invalidateInverse: Unit =
    _inverseCache.clear
  def invalidateInverse[P <: Record[P], C <: Record[C]](inverse: InverseAssociation[P, C]): Unit =
    _inverseCache.remove(inverse)
  def getInverse[P <: Record[P], C <: Record[C]](inverse: InverseAssociation[P, C],
                                                 parentId: Long): Option[Seq[C]] =
    _inverseCache(inverse).get(parentId).asInstanceOf[Option[Seq[C]]]
  def updateInverse[P <: Record[P], C <: Record[C]](inverse: InverseAssociation[P, C],
                                                    parentId: Long,
                                                    children: Seq[C]): Unit =
    _inverseCache(inverse).update(parentId, children)
  def evictInverse[P <: Record[P], C <: Record[C]](inverse: InverseAssociation[P, C],
                                                   parentId: Long): Unit =
    _inverseCache(inverse).remove(parentId)

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