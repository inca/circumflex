package ru.circumflex.orm

import core.{Messages, GroupBy, CircumflexException}

/**
 * Represents a validation error. It's <code>source</code> describes
 * a context where validation has failed (it can be column name, association, etc.)
 * <code>errorKey</code> specifies, what problem has occured.
 */
case class ValidationError(val source: String,
                           val errorKey: String,
                           var params: Map[String, String]) {

  def this(source: String, errorKey: String) = this(source, errorKey, Map[String, String]())

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
}

class ValidationException(val errors: ValidationError *)
    extends CircumflexException("Validation failed.") {
  private val _errorsMap = GroupBy.apply[String, ValidationError](errors, e => e.source)
  def errorsMap = _errorsMap
}

abstract class Validator(val source: String) {
  def apply(value: Any): Option[ValidationError]
}

class NotNullValidator(source: String) extends Validator(source) {

  def apply(value: Any) = value match {
    case Some(value) => apply(value)
    case null | None => Some(new ValidationError(source, "validation.null"))
    case _ => None
  }

}

class NotEmptyValidator(source: String) extends NotNullValidator(source) {

  override def apply(value: Any) = super.apply(value) match {
    case None => value match {
      case c: Collection[_] if (c.size == 0) => Some(new ValidationError(source, "validation.empty"))
      case s: String if (s.trim.length == 0) => Some(new ValidationError(source, "validation.empty"))
      case _ => None
    }
    case value => value
  }

}

class PatternValidator(source: String, regex: String) extends NotNullValidator(source) {

  override def apply(value: Any) = super.apply(value) match {
    case None => if (value.toString.matches(regex)) None
    else Some(new ValidationError(
      source, "validation.pattern", "value" -> value.toString, "pattern" -> regex))
    case value => value
  }

}

abstract class RecordValidator[R] {
  def apply(record: Record[R]): Option[ValidationError]
}

class RecordFieldValidator[R](val column: Column[_, R], validator: Validator)
    extends RecordValidator[R] {

  def apply(record: Record[R]) =
    validator.apply(record.fieldsMap.get(column))

}