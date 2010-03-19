package ru.circumflex.orm

import ru.circumflex.core.{Messages, CircumflexUtil, CircumflexException}

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

  def toMsg(): String = toMsg(Messages())
}

class ValidationException(val errors: ValidationError *)
    extends CircumflexException("Validation failed.") {
  private val _errorsMap = CircumflexUtil.groupBy[String, ValidationError](errors, e => e.source)
  def errorsMap = _errorsMap
}

abstract class Validator(val source: String) {
  def apply(value: Any): Option[ValidationError] = value match {
    case Some(value) => apply(value)
    case value => validate(value)
  }

  def validate(value: Any): Option[ValidationError]
}

class NotNullValidator(source: String) extends Validator(source) {

  def validate(value: Any) = value match {
    case null | None => Some(new ValidationError(source, "validation.null"))
    case _ => None
  }

}

class NotEmptyValidator(source: String) extends Validator(source) {

  def validate(value: Any) = value match {
    case c: scala.Collection[_] if (c.size == 0) =>
      Some(new ValidationError(source, "validation.empty"))
    case s: String if (s.trim.length == 0) =>
      Some(new ValidationError(source, "validation.empty"))
    case _ => None
  }

}

class PatternValidator(source: String, regex: String) extends Validator(source) {

  def validate(value: Any) = value match {
    case s: String if s.matches(regex) => None
    case v => Some(new ValidationError(
      source, "validation.pattern", "value" -> v.toString, "pattern" -> regex))
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
