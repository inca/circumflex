package ru.circumflex.orm

import _root_.ru.circumflex.core._
import util.matching.Regex

// ## Validation

case class ValidationError(val source: String,
                           val errorKey: String,
                           var params: Map[String, String]) {
  def this(source: String, errorKey: String, params: Pair[String, String]*) =
    this(source, errorKey, Map(params: _*))

  params += "src" -> source

  def toMsg(messages: Messages): String =
    messages.get(source + "." + errorKey, params) match {
      case Some(m) => m
      case None => messages.get(errorKey, params) match {
        case Some(m) => m
        case None => errorKey
      }
    }
  def toMsg(): String = toMsg(CircumflexContext.get.messages)

  override def hashCode = source.hashCode * 31 + errorKey.hashCode
  override def equals(that: Any) = that match {
    case e: ValidationError => e.source == this.source && e.errorKey == this.errorKey
    case _ => false
  }
}

class ValidationException(val errors: ValidationError *)
    extends CircumflexException("Validation failed.") {
  val bySource = CircumflexUtil
      .groupBy[String, ValidationError](errors, e => e.source)
  val byKey = CircumflexUtil
      .groupBy[String, ValidationError](errors, e => e.errorKey)
}

class RecordValidator[R <: Record[R]](record: R) {
  protected var _validators: Seq[R => Option[ValidationError]] = Nil
  def validators = _validators
  def validate(): Seq[ValidationError] =
    _validators.flatMap(_.apply(record)).toList.removeDuplicates
  def add(validator: R => Option[ValidationError]): this.type = {
    _validators ++= List(validator)
    return this
  }
  def notNull(field: Field[_]): this.type =
    add(r => if (field.null_?)
      Some(new ValidationError(field.uuid, "null"))
    else None)
  def notEmpty(field: TextField): this.type =
    add(r => if (field.null_?)
      Some(new ValidationError(field.uuid, "null"))
    else if (field.getValue.trim == "")
      Some(new ValidationError(field.uuid, "empty"))
    else None)
  def pattern(field: TextField, regex: String, key: String = "pattern"): this.type =
    add(r => if (field.null_?)
      Some(new ValidationError(field.uuid, "null"))
    else if (!field.getValue.matches(regex))
      Some(new ValidationError(field.uuid, key, "regex" -> regex, "value" -> field.getValue))
    else None)
}



