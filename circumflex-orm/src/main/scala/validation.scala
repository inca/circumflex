package ru.circumflex.orm

import ru.circumflex.core._

class ValidationException(val errors: MsgGroup)
    extends ORMException("Record validation failed.")

class RecordValidator[PK, R <: Record[PK, R]] {

  protected var _validators: Seq[R => Option[Msg]] = Nil

  def validators = _validators

  def validate(record: R): Seq[Msg] =
    _validators.flatMap(_.apply(record)).toList.distinct

  def add(validator: R => Option[Msg]): this.type = {
    _validators ++= List(validator)
    return this
  }

  def addForTransient(validator: R => Option[Msg]): this.type =
    add(r => if (r.transient_?) validator(r) else None)

  def addForPersisted(validator: R => Option[Msg]): this.type =
    add(r => if (!r.transient_?) validator(r) else None)

  def notNull(f: R => Field[_, R]): this.type = add { r =>
    val field = f(r)
    if (field.null_?)
      Some(new Msg(field.uuid + ".null", "record" -> r, "field" -> field))
    else None
  }

  def notEmpty(f: R => TextField[R]): this.type = add { r =>
    val field = f(r)
    if (field.null_?)
      Some(new Msg(field.uuid + ".null", "record" -> r, "field" -> field))
    else if (field().trim == "")
      Some(new Msg(field.uuid + ".empty", "record" -> r, "field" -> field))
    else None
  }

  def pattern(f: R => TextField[R], regex: String, key: String = "pattern"): this.type = add { r =>
    val field = f(r)
    if (field.null_?)
      None
    else if (!field().matches(regex))
      Some(new Msg(field.uuid + "." + key, "regex" -> regex,
        "value" -> field(),
        "record" -> r,
        "field" -> field))
    else None
  }

  def unique(f: R => Field[_, R], key: String = "unique"): this.type = addForTransient { r =>
    val field = f(r)
    r.relation.criteria.add(field EQ field()).unique.map { a =>
      new Msg(field.uuid + "." + key, "record" -> r, "field" -> field)
    }
  }

  def uniqueAll(f: R => Seq[Field[_, R]], key: String = "unique"): this.type = addForTransient { r =>
    val fields = f(r)
    val crit = r.relation.criteria
    fields.foreach(f => crit.add(f EQ f()))
    crit.unique.map(a => new Msg(r.uuid + "." + key, "record" -> r))
  }

}