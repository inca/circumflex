package ru.circumflex.orm

import core.{Messages, GroupBy}

/**
 * Represents a validation error. It's <code>source</code> describes
 * a context where validation has failed (it can be column name, association, etc.)
 * <code>errorKey</code> specifies, what problem has occured.
 */
case class ValidationError(val source: String,
                           val errorKey: String,
                           val params: collection.Map[String, String]) {

  def this(source: String, errorKey: String) = this(source, errorKey, Map())

  def toMsg(messages: Messages): String =
    messages.apply(source + "." + errorKey, params) match {
      case Some(m) => m
      case None => messages.apply(errorKey, params) match {
        case Some(m) => m
        case None => errorKey
      }
    }

}

class ValidationException(val errors: ValidationError *) {

  def errorsMap = GroupBy.apply[String, ValidationError](errors, e => e.source)

}