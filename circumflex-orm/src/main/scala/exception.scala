package ru.circumflex.orm

/*!# Exceptions

The `ORMException` are thrown by Circumflex ORM components and usually mean that current
transaction needs to be rolled back and closed ASAP.
*/

/**
 * General exception in Circumflex ORM.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0/circumflex-orm/exception.scala">exception.scala</a>.
 */
class ORMException(msg: String, cause: Throwable) extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)
  def this(cause: Throwable) = this(null, cause)
}