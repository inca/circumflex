package ru.circumflex
package orm

import java.util.concurrent.atomic._

/*!# ORM Statistics

The `Statistics` singleton is used to gather the statistical information from
various code points inside Circumflex ORM. Most methods rely on non-blocking
counters from `java.util.concurrent.atomic` package.

Following statistical data is available:

  * `connectionsOpened` -- the amount of physical JDBC connections opened by
  `Transaction` objects;
  * `connectionsClosed` -- the amount of physical JDBC connections closed by
  `Transaction` objects;
  * `executions` -- the total amount of invokations of the `execute` method of
  `Transaction` objects
  * `executionsSucceeded` -- the amount of successful invokations of the
  `execute` method of `Transaction` objects;
  * `executionsFailed` -- the amount of failed invokations of the `execute`
  method of `Transaction` objects;
  * `recordCacheHits` -- the amount of records successfully retrieved from cache;
  * `recordCacheMisses` -- the amount of records retrieved from database and stored
  in cache;
  * `inverseCacheHits` -- the amount of inverse associations retrieved from cache;
  * `inverseCacheMisses` -- the amount of inverse associations retrieved from database
  and stored in cache;
  * `heaviestSqlTime` -- the time (in milliseconds) of the heaviest SQL query execution;
  * `heaviestSql` -- the SQL statement of the heaviest data-retrieval query.
*/
class StatisticsManager {

  val connectionsOpened = new AtomicInteger(0)
  val connectionsClosed = new AtomicInteger(0)
  val executions = new AtomicInteger(0)
  val executionsSucceeded = new AtomicInteger(0)
  val executionsFailed = new AtomicInteger(0)
  val recordCacheHits = new AtomicInteger(0)
  val recordCacheMisses = new AtomicInteger(0)
  val inverseCacheHits = new AtomicInteger(0)
  val inverseCacheMisses = new AtomicInteger(0)

  protected var _heaviestSqlTime = 0l
  def heaviestSqlTime = _heaviestSqlTime

  protected var _heaviestSql = ""
  def heaviestSql = _heaviestSql

  def executeSql(q: SQLQuery[_]) {
    synchronized {
      val t = q.executionTime
      if (t > _heaviestSqlTime) {
        _heaviestSqlTime = t
        _heaviestSql = q.toInlineSql
      }
    }
  }

  protected var _heaviestDmlTime = 0l
  def heaviestDmlTime = _heaviestDmlTime

  protected var _heaviestDml = ""
  def heaviestDml = _heaviestDml

  def executeDml(q: DMLQuery) {
    synchronized {
      val t = q.executionTime
      if (t > _heaviestDmlTime) {
        _heaviestDmlTime = t
        _heaviestDml = q.toInlineSql
      }
    }
  }

  def clear() {
    synchronized {
      connectionsOpened.set(0)
      connectionsClosed.set(0)
      executions.set(0)
      executionsSucceeded.set(0)
      executionsFailed.set(0)
      recordCacheHits.set(0)
      recordCacheMisses.set(0)
      inverseCacheHits.set(0)
      inverseCacheMisses.set(0)
      _heaviestSql = ""
      _heaviestSqlTime = 0l
      _heaviestDml = ""
      _heaviestDmlTime = 0l
    }
  }

}
