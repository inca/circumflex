package ru.circumflex
package orm

import collection.mutable.HashMap
import net.sf.ehcache._
import java.util.concurrent.atomic._

/*!# Context-Level Cache

The `CacheService` trait defines minimum functionality required for organizing
context-level cache.

The context-level cache is designed to maintain records within a single
transaction. This functionality is required for all data-retrieval operations.

The cache consists of two logical parts:

  1. _record cache_ holds individual records by their relations and `id`s;
  2. _inverse cache_ holds sequences of records by their associations and their parent's `id`s.
*/
trait CacheService {

  def invalidate() {
    invalidateRecords()
    invalidateInverse()
  }

  /*!## Records Cache

  Following methods are used to maintain records cache:

  * `invalidateRecords` clears all records from cache or only those who correspond
  to specified `relation`;
  * `getRecord` retrieves a record from cache by specified `relation` and `id`;
  * `updateRecord` updates a cache with specified `record`;
  * `evictRecord` removes a record from cache by specified `relation` and `id`.
  */
  def invalidateRecords()
  def invalidateRecords[PK, R <: Record[PK, R]](
      relation: Relation[PK, R])
  def cacheRecord[PK, R <: Record[PK, R]](
      id: PK, relation: Relation[PK, R], record: => Option[R]): Option[R]
  def evictRecord[PK, R <: Record[PK, R]](
      id: PK, relation: Relation[PK, R])
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
  * `evictInverse` removes children from inverse cache by specified `association` and `parentId`;
  */
  def invalidateInverse()
  def invalidateInverse[K, C <: Record[_, C], P <: Record[K, P]](
      association: Association[K, C, P])
  def cacheInverse[K, C <: Record[_, C], P <: Record[K, P]](
      parentId: K, association: Association[K, C, P], children: => Seq[C]): Seq[C]
  def evictInverse[K, C <: Record[_, C], P <: Record[K, P]](
      parentId: K, association: Association[K, C, P])
  def updateInverse[K, C <: Record[_, C], P <: Record[K, P]](
      parentId: K, association: Association[K, C, P], children: Seq[C]): Seq[C] = {
    evictInverse(parentId, association)
    cacheInverse(parentId, association, children)
  }
  def evictInverse[K, P <: Record[K, P]](parent: P)

}

/*! The default cache service implementation relies on Scala mutable `HashMap`s.
It can be overriden by setting the `orm.contextCache` parameter. */
class DefaultCacheService extends CacheService {

  class CacheMap extends HashMap[Any, HashMap[Any, Any]] {
    override def apply(key: Any): HashMap[Any, Any] =
      super.getOrElseUpdate(key, new HashMap[Any, Any])
  }

  val _recordsCache = new CacheMap
  val _inverseCache = new CacheMap

  // Records cache

  def invalidateRecords() {
    _recordsCache.clear()
    Cacheable.relations.foreach(_.invalidateCache())
  }
  def invalidateRecords[PK, R <: Record[PK, R]](relation: Relation[PK, R]) {
    relation match {
      case c: Cacheable[_, _] => c.invalidateCache()
      case _ => _recordsCache.remove(relation)
    }
  }
  def evictRecord[PK, R <: Record[PK, R]](id: PK, relation: Relation[PK, R]) {
    relation match {
      case c: Cacheable[_, _] => c.evict(id)
      case _ => _recordsCache(relation).remove(id)
    }
  }
  def cacheRecord[PK, R <: Record[PK, R]](
      id: PK, relation: Relation[PK, R], record: => Option[R]): Option[R] =
    relation match {
      case c: Cacheable[PK, R] => c.cache(id, record)
      case _ =>
        val c = _recordsCache(relation)
        c.get(id).map { r =>
          ormConf.statisticsManager.recordCacheHits.incrementAndGet()
          r.asInstanceOf[R]
        } orElse {
          ormConf.statisticsManager.recordCacheMisses.incrementAndGet()
          val v = record
          v.map { r =>
            c.update(id, r)
            r
          }
        }
    }

  // Inverse cache

  def invalidateInverse() {
    _inverseCache.clear()
  }
  def invalidateInverse[K, C <: Record[_, C], P <: Record[K, P]](
      association: Association[K, C, P]) {
    _inverseCache(association).clear()
  }
  def cacheInverse[K, C <: Record[_, C], P <: Record[K, P]](
      parentId: K, association: Association[K, C, P], children: => Seq[C]): Seq[C] = {
    val cache = _inverseCache(association)
    cache.get(parentId) match {
      case Some(children: Seq[C]) =>
        ormConf.statisticsManager.inverseCacheHits.incrementAndGet()
        children
      case _ =>
        ormConf.statisticsManager.inverseCacheMisses.incrementAndGet()
        val c = children
        cache.update(parentId, c)
        c
    }
  }
  def evictInverse[K, C <: Record[_, C], P <: Record[K, P]](
      parentId: K, association: Association[K, C, P]) {
    _inverseCache(association).remove(parentId)
  }
  def evictInverse[K, P <: Record[K, P]](
      parent: P) {
    _inverseCache.keys.foreach {
      case a: Association[K, _, P] =>
        if (a.parentRelation == parent.relation)
          _inverseCache(a).remove(parent.PRIMARY_KEY())
      case _ =>
    }
  }

}

/*!# Application-Level Cache

Circumflex ORM lets you organize application-scope cache (backed by Terracotta Ehcache)
for any relation of your application: just mix in the `Cacheable` trait into your relation.
Note that since one record instance may become accessible to several threads, the
modification of such records is a subject for concurrency control.
*/
trait Cacheable[PK, R <: Record[PK, R]] extends Relation[PK, R] { this: R =>

  def cacheName = ormConf.prefix(":") + qualifiedName

  def ehcache: Ehcache = ru.circumflex.cache.ehcacheManager
      .addCacheIfAbsent(cacheName)

  // Per-relation statistics
  val cacheHits = new AtomicInteger(0)
  val cacheMisses = new AtomicInteger(0)

  def cache(id: PK, record: => Option[R]): Option[R] = {
    var elem = ehcache.get(id)
    if (elem == null) {
      elem = new Element(id, record)
      ehcache.put(elem)
      cacheMisses.incrementAndGet()
      ormConf.statisticsManager.recordCacheMisses.incrementAndGet()
    } else {
      cacheHits.incrementAndGet()
      ormConf.statisticsManager.recordCacheHits.incrementAndGet()
    }
    elem.getValue.asInstanceOf[Option[R]]
  }
  def evict(id: PK) {
    ehcache.remove(id)
  }
  def invalidateCache() {
    ehcache.removeAll()
  }

  afterInsert(r => cache(r.PRIMARY_KEY(), Some(r)))
  afterUpdate(r => cache(r.PRIMARY_KEY(), Some(r)))
  afterDelete(r => evict(r.PRIMARY_KEY()))

  Cacheable.add(this)
}

object Cacheable {
  private var _relations: Seq[Cacheable[_, _]] = Nil
  def relations = _relations
  def add[PK, R <: Record[PK, R]](relation: Cacheable[PK, R]): this.type = {
    _relations ++= List(relation)
    this
  }
}