package ru.circumflex.core

/*!# Exception

All exceptions thrown from Circumflex components should extend `CircumflexException` class.
*/

/**
 * Indicated a common error occured inside Circumflex component.
 */
class CircumflexException(msg: String, cause: Throwable = null)
    extends RuntimeException(msg, cause) {
  def this(cause: Throwable) = this(null, cause)
}