/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

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

  def toMsg(): String = toMsg(Messages())
}

class ValidationException(val errors: ValidationError *)
    extends CircumflexException("Validation failed.") {
  private val _errorsMap = GroupBy.apply[String, ValidationError](errors, e => e.source)
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
    case c: Collection[_] if (c.size == 0) => Some(new ValidationError(source, "validation.empty"))
    case s: String if (s.trim.length == 0) => Some(new ValidationError(source, "validation.empty"))
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
