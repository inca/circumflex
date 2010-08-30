package ru.circumflex.core

/*!# Logger

Internally Circumflex uses [slf4j](http://www.slf4j.org) to log it's activities.

The `Logger` class lets applications save some performance by introducing by-name logging.
*/
class Logger(val name: String) {
  protected val log = org.slf4j.LoggerFactory.getLogger(name)
  def info(msg: => String): Unit = if (log.isInfoEnabled) log.info(msg)
  def warn(msg: => String): Unit = if (log.isWarnEnabled) log.warn(msg)
  def warn(msg: => String, e: Throwable): Unit = if (log.isWarnEnabled) log.warn(msg, e)
  def error(msg: => String, e: Throwable): Unit = if (log.isErrorEnabled) log.error(msg, e)
  def error(msg: => String): Unit = if (log.isErrorEnabled) log.error(msg)
  def debug(msg: => String, e: Throwable): Unit = if (log.isDebugEnabled) log.debug(msg, e)
  def debug(msg: => String): Unit = if (log.isDebugEnabled) log.debug(msg)
  def trace(msg: => String, e: Throwable): Unit = if (log.isTraceEnabled) log.trace(msg, e)
  def trace(msg: => String): Unit = if (log.isTraceEnabled) log.trace(msg)
}