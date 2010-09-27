package ru.circumflex.orm

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
  *

*/

object Statistics {

  protected val _connectionsOpened = new AtomicInteger(0)
  def connectionsOpened = _connectionsOpened.get
  def connectionOpen = _connectionsOpened.incrementAndGet
  
  protected val _connectionsClosed = new AtomicInteger(0)
  def connectionsClosed = _connectionsClosed.get
  def connectionClose = _connectionsClosed.incrementAndGet

  protected val _executions = new AtomicInteger(0)
  def executions = _executions.get
  def execution = _executions.incrementAndGet

  protected val _executionsSucceeded = new AtomicInteger(0)
  def executionsSucceeded = _executionsSucceeded.get
  def executionSucceeded = _executionsSucceeded.incrementAndGet

  protected val _executionsFailed = new AtomicInteger(0)
  def executionsFailed = _executionsFailed.get
  def executionFailed = _executionsFailed.incrementAndGet

}
