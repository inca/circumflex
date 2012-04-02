package ru.circumflex
package core

/*!# Exception

All exceptions thrown from Circumflex components should extend
`CircumflexException` class.
*/
class CircumflexException(msg: String, cause: Throwable = null)
    extends RuntimeException(msg, cause) {
  def this(cause: Throwable) = this(null, cause)
}

class ValidationException(val errors: Seq[Msg])
    extends CircumflexException("Validation failed: " + errors) {
  def this(msg: Msg) = this(List(msg))
  def this(msg: Msg, msgs: Msg*) = this(List(msg) ++ msgs.toSeq)
  def this(key: String) = this(new Msg(key))
  def this(key: String, keys: String*) =
    this(List(key).++(keys).map(k => new Msg(k)))
}