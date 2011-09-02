package ru.circumflex
package orm

import core._

class RecordValidator[PK, R <: Record[PK, R]] {

  protected var _validators: Seq[R => Option[Msg]] = Nil

  def validators = _validators

  def validate(record: R): Seq[Msg] =
    _validators.flatMap(_.apply(record)).toList.distinct

  def add(validator: R => Option[Msg]): this.type = {
    _validators ++= List(validator)
    this
  }

  def addForTransient(validator: R => Option[Msg]): this.type =
    add(r => if (r.isTransient) validator(r) else None)

  def addForPersisted(validator: R => Option[Msg]): this.type =
    add(r => if (!r.isTransient) validator(r) else None)

  def notNull(f: R => Field[_, R]): this.type = add { r =>
    val field = f(r)
    if (field.isEmpty)
      Some(new Msg(field.uuid + ".null", "record" -> r, "field" -> field))
    else None
  }

  def notEmpty(f: R => TextField[R]): this.type = add { r =>
    val field = f(r)
    if (field.isEmpty)
      Some(new Msg(field.uuid + ".null", "record" -> r, "field" -> field))
    else if (field().trim == "")
      Some(new Msg(field.uuid + ".empty", "record" -> r, "field" -> field))
    else None
  }

  def pattern(f: R => TextField[R], regex: String, key: String = "pattern"): this.type = add { r =>
    val field = f(r)
    if (field.isEmpty)
      None
    else if (!field().matches(regex))
      Some(new Msg(field.uuid + "." + key, "regex" -> regex,
        "value" -> field(),
        "record" -> r,
        "field" -> field))
    else None
  }

  def unique[T](f: R => Field[T, R], key: String = "unique"): this.type = add { r =>
    val field = f(r)
    r.relation.criteria.add(field EQ field()).unique() match {
      case Some(a) if (r.isTransient || a != r) =>
        Some(new Msg(field.uuid + "." + key, "record" -> r, "field" -> field))
      case _ => None
    }
  }

  def uniqueAll(f: R => Seq[Field[_, R]], key: String = "unique"): this.type = add { r =>
    val fields = f(r)
    val crit = r.relation.criteria
    fields.foreach {
      case f: Field[Any, R] => crit.add(f EQ f())
      case _ =>
    }
    crit.unique() match {
      case Some(a: R) if (r.isTransient || a != r) =>
         Some(new Msg(r.uuid + "." + key, "record" -> r))
      case _ => None
    }
  }

}