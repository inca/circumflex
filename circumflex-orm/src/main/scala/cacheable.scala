package ru.circumflex.orm

import ru.circumflex.core._
import java.util.concurrent.ConcurrentHashMap

/**
 * Preserves every record in it's own application-scoped cache.
 * This cache spans across multiple transactions and is invalidated either manually
 * by invoking `invalidateCache` method, or implicitly by invoking `insert_!`, `update!`
 * or `delete_!` method on corresponding record.
 *
 * **Warning!** We see no way of invalidating application-scope cache in response to custom DML queries,
 * so if you writing data into cacheable relations with other methods, then `insert_!`, `update_!`
 * or `delete_!` will **always** result in cache inconsistencies. Use at your own risk!
 */
trait Cacheable[R <: Record[R]] extends Relation[R] {
  protected var _cache = new ConcurrentHashMap[Long, R]

  def updateRecordCache(record: R): R = {
    _cache.put(record.id(), record)
    return record
  }
  def getCachedRecord(id: Long): Option[R] = _cache.get(id)
  def evictRecordCache(record: R): Unit = _cache.remove(record.id())

  /**
   * Clears applciation-scoped cache.
   */
  def invalidateCache(): Unit = {
    _cache = new ConcurrentHashMap[Long, R]
  }

  afterInsert(r => invalidateCache)
  afterUpdate(r => invalidateCache)
  afterDelete(r => invalidateCache)

}
