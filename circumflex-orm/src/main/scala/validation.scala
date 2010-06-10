package ru.circumflex.orm

import _root_.ru.circumflex.core._
import java.lang.String

// ## Validation

case class ValidationError(val source: String,
                           val errorKey: String,
                           var params: Map[String, String]) {
  def this(source: String, errorKey: String, params: Pair[String, String]*) =
    this(source, errorKey, Map(params: _*))

  params += "src" -> source

  protected def toMsg(key: String, messages: Messages): String = messages.get(key, params) match {
    case Some(msg) => msg
    case _ =>
      val i = key.indexOf(".")
      if (i == -1) key
      else toMsg(key.substring(i + 1), messages)
  }
  def toMsg(messages: Messages): String = toMsg(source + "." + errorKey, messages)
  def toMsg(): String = toMsg(CircumflexContext.get.messages)

  protected def matches(thisKey: String, key: String): Boolean =
    if (thisKey == key || thisKey + "." + errorKey == key) true
    else {
      val i = thisKey.indexOf(".")
      if (i == -1) false
      else matches(thisKey.substring(i + 1), key)
    }

  def matches(key: String): Boolean = matches(source, key)

  override def hashCode = source.hashCode * 31 + errorKey.hashCode
  override def equals(that: Any) = that match {
    case e: ValidationError => e.source == this.source && e.errorKey == this.errorKey
    case _ => false
  }
}

class ValidationException(val errors: Seq[ValidationError])
    extends CircumflexException("Validation failed.") with HashModel {
  def get(key: String): Option[Seq[ValidationError]] = Some(errors.filter(e => e.matches(key)))
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
      None
    else if (!field.getValue.matches(regex))
      Some(new ValidationError(field.uuid, key, "regex" -> regex, "value" -> field.getValue))
    else None)
}



