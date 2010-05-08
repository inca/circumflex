package ru.circumflex.orm

import ru.circumflex.core.{CircumflexUtil, CircumflexException, Messages}

// ## Validation

case class ValidationError(val source: String,
                           val errorKey: String,
                           var params: Map[String, String]) {
  def this(source: String, errorKey: String) =
    this(source, errorKey, Map[String, String]())
  def this(source: String, errorKey: String, params: Pair[String, String]*) =
    this(source, errorKey, Map(params: _*))

  params += "src" -> source

  def toMsg(messages: Messages): String =
    messages.apply(source + "." + errorKey, params) match {
      case Some(m) => m
      case None => messages.apply(errorKey, params) match {
        case Some(m) => m
        case None => errorKey
      }
    }
  def toMsg(): String = toMsg(Messages())
}

class ValidationException(val errors: ValidationError *)
    extends CircumflexException("Validation failed.") {
  val errorsMap = CircumflexUtil
      .groupBy[String, ValidationError](errors, e => e.source)
}



