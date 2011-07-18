package ru.circumflex
package core

/*!# Logger

Internally Circumflex uses [slf4j](http://www.slf4j.org) to log it's activities.

The `Logger` class lets applications save some performance by introducing by-name logging.
*/
class Logger(val name: String) {
  protected val log = org.slf4j.LoggerFactory.getLogger(name)
  def info(msg: => Any) {
    if (log.isInfoEnabled) log.info(msg.toString)
  }
  def warn(msg: => Any) {
    if (log.isWarnEnabled) log.warn(msg.toString)
  }
  def warn(msg: => Any, e: Throwable) {
    if (log.isWarnEnabled) log.warn(msg.toString, e)
  }
  def error(msg: => Any, e: Throwable) {
    if (log.isErrorEnabled) log.error(msg.toString, e)
  }
  def error(msg: => Any) {
    if (log.isErrorEnabled) log.error(msg.toString)
  }
  def debug(msg: => Any, e: Throwable) {
    if (log.isDebugEnabled) log.debug(msg.toString, e)
  }
  def debug(msg: => Any) {
    if (log.isDebugEnabled) log.debug(msg.toString)
  }
  def trace(msg: => Any, e: Throwable) {
    if (log.isTraceEnabled) log.trace(msg.toString, e)
  }
  def trace(msg: => Any) {
    if (log.isTraceEnabled) log.trace(msg.toString)
  }
}