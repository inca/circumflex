package circumflex
package core

/*!# Logger

Internally Circumflex uses [slf4j](http://www.slf4j.org) to log its activities.

The `Logger` class lets applications save some performance by introducing
by-name logging.
*/
class Logger(val name: String) {

  protected val log = org.slf4j.LoggerFactory.getLogger(name)

  def isError = log.isErrorEnabled

  def error(msg: => Any, e: Throwable) {
    if (isError) log.error(msg.toString, e)
  }

  def error(msg: => Any) {
    if (isError) log.error(msg.toString)
  }

  def isWarn = log.isWarnEnabled

  def warn(msg: => Any) {
    if (isWarn) log.warn(msg.toString)
  }

  def warn(msg: => Any, e: Throwable) {
    if (isWarn) log.warn(msg.toString, e)
  }

  def isInfo = log.isInfoEnabled

  def info(msg: => Any) {
    if (isInfo) log.info(msg.toString)
  }

  def isDebug = log.isDebugEnabled

  def debug(msg: => Any, e: Throwable) {
    if (isDebug) log.debug(msg.toString, e)
  }

  def debug(msg: => Any) {
    if (isDebug) log.debug(msg.toString)
  }

  def isTrace = log.isTraceEnabled

  def trace(msg: => Any, e: Throwable) {
    if (isTrace) log.trace(msg.toString, e)
  }

  def trace(msg: => Any) {
    if (isTrace) log.trace(msg.toString)
  }
}