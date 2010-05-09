package ru.circumflex.orm

import ru.circumflex.core.{CircumflexUtil, CircumflexException, Messages}
import util.matching.Regex

// ## Validation

case class ValidationError(val source: String,
                           val errorKey: String,
                           var params: Map[String, String]) {
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
    add(r => r._fields.find(f => f == field) match {
      case Some(f: Field[_]) =>
        if (f.null_?) Some(new ValidationError(f.uuid, "null"))
        else None
      case None =>
        throw new ORMException("Field " + field + " does not correspond to record " + record.uuid)
    })
  def notEmpty(field: TextField): this.type =
    add(r => r._fields.find(f => f == field) match {
      case Some(f: TextField) =>
        if (f.getValue == null) Some(new ValidationError(f.uuid, "null"))
        else if (f.getValue.trim == "") Some(new ValidationError(f.uuid, "empty"))
        else None
      case None =>
        throw new ORMException("Field " + field + " does not correspond to record " + record.uuid)
    })
  def pattern(field: TextField, regex: String, key: String = "pattern"): this.type =
    add(r => r._fields.find(f => f == field) match {
      case Some(f: TextField) =>
        if (f.getValue == null) Some(new ValidationError(f.uuid, "null"))
        else if (!f.getValue.matches(regex))
          Some(new ValidationError(f.uuid, key, "regex" -> regex, "value" -> f.getValue))
        else None
      case None =>
        throw new ORMException("Field " + field + " does not correspond to record " + record.uuid)
    })
}



