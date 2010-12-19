package ru.circumflex.core

/*!# Exception

All exceptions thrown from Circumflex components should extend `CircumflexException` class.
*/

/**
 * Indicates a common error occured inside Circumflex component.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0.RC3/circumflex-core/exception.scala">exception.scala</a>.
 */
class CircumflexException(msg: String, cause: Throwable = null)
    extends RuntimeException(msg, cause) {
  def this(cause: Throwable) = this(null, cause)
}