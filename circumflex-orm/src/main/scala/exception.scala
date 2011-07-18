package ru.circumflex
package orm

/*!# Exceptions

The `ORMException`s are thrown by Circumflex ORM components and usually mean that current
transaction needs to be rolled back and closed ASAP.
*/
class ORMException(msg: String, cause: Throwable) extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)
  def this(cause: Throwable) = this(null, cause)
}