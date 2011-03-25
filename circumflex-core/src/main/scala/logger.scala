package ru.circumflex.core

/*!# Logger

Internally Circumflex uses [slf4j](http://www.slf4j.org) to log it's activities.

The `Logger` class lets applications save some performance by introducing by-name logging.
*/

/**
 * A helper for logging.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0.2/circumflex-core/logger.scala">logger.scala</a>.
 */
class Logger(val name: String) {
  protected val log = org.slf4j.LoggerFactory.getLogger(name)
  def info(msg: => Any): Unit = if (log.isInfoEnabled) log.info(msg.toString)
  def warn(msg: => Any): Unit = if (log.isWarnEnabled) log.warn(msg.toString)
  def warn(msg: => Any, e: Throwable): Unit = if (log.isWarnEnabled) log.warn(msg.toString, e)
  def error(msg: => Any, e: Throwable): Unit = if (log.isErrorEnabled) log.error(msg.toString, e)
  def error(msg: => Any): Unit = if (log.isErrorEnabled) log.error(msg.toString)
  def debug(msg: => Any, e: Throwable): Unit = if (log.isDebugEnabled) log.debug(msg.toString, e)
  def debug(msg: => Any): Unit = if (log.isDebugEnabled) log.debug(msg.toString)
  def trace(msg: => Any, e: Throwable): Unit = if (log.isTraceEnabled) log.trace(msg.toString, e)
  def trace(msg: => Any): Unit = if (log.isTraceEnabled) log.trace(msg.toString)
}