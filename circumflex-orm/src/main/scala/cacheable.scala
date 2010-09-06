package ru.circumflex.orm

import ru.circumflex.core._
import java.util.concurrent.ConcurrentHashMap

/**
 * Preserves every record in it's own application-scoped cache.
 *
 * **Warning!** We see no way of invalidating application-scope cache in response to custom DML queries,
 * so writing data into cacheable relations with other methods, than `insert_!`, `update_!`
 * or `delete_!`, will **always** result in cache inconsistencies. Use at your own risk!
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
