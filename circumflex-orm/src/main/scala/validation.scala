package ru.circumflex.orm

import ru.circumflex.core._

/*!# Validation

TODO document
*/

class ValidationException(val errors: MsgGroup) extends CircumflexException("Validation failed.")

class RecordValidator[R <: Record[R]] {
  protected var _validators: Seq[R => Option[Msg]] = Nil

  def validators = _validators

  def validate(record: R): Seq[Msg] =
    _validators.flatMap(_.apply(record)).toList.removeDuplicates

  def add(validator: R => Option[Msg]): this.type = {
    _validators ++= List(validator)
    return this
  }
  def addForInsert(validator: R => Option[Msg]): this.type =
    add(r => if (r.transient_?) validator(r) else None)
  def addForUpdate(validator: R => Option[Msg]): this.type =
    add(r => if (!r.transient_?) validator(r) else None)

  def notNull(f: R => Field[_]): this.type = add(r => {
    val field = f(r)
    if (field.null_?) Some(new Msg(field.uuid + ".null")) else None
  })
  def notEmpty(f: R => TextField): this.type = add(r => {
    val field = f(r)
    if (field.null_?)
      Some(new Msg(field.uuid + ".null"))
    else if (field.getValue.trim == "")
      Some(new Msg(field.uuid + ".empty"))
    else None
  })
  def pattern(f: R => TextField, regex: String, key: String = "pattern"): this.type = add(r => {
    val field = f(r)
    if (field.null_?)
      None
    else if (!field.getValue.matches(regex))
      Some(new Msg(field.uuid + "." + key, "regex" -> regex, "value" -> field.getValue))
    else None
  })
  def unique(f: R => Field[_], key: String = "unique"): this.type = addForInsert(r => {
    val field = f(r)
    r.relation.criteria.add(field EQ field()).unique.map(a => new Msg(field.uuid + "." + key))
  })
  def uniqueAll(f: R => Seq[Field[_]], key: String = "unique"): this.type = addForInsert(r => {
    val fields = f(r)
    val crit = r.relation.criteria
    fields.foreach(f => crit.add(f EQ f()))
    crit.unique.map(a => new Msg(r.uuid + "." + key))
  })
}



