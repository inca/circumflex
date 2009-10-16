package circumflex.hibernate

import circumflex.core.{HttpResponse,GroupBy}
import javax.validation.ConstraintViolation


trait HibernateHelper extends TransactionContext {

  def tx(actions: HibernateSession => HttpResponse)
        (error: Throwable => HttpResponse) = transaction(actions)(error)

  def validate[T](obj: T): ValidationResult = {
    val set = provider.validator.validate(obj)
    if (set.size == 0) Valid
    else NotValid[T](set.toArray.map(_.asInstanceOf[ConstraintViolation[T]]))
  }

}


trait ValidationResult

object Valid extends ValidationResult

case class NotValid[T](val violations: Seq[ConstraintViolation[T]]) extends ValidationResult {

  val byProperties: collection.Map[String, Seq[ConstraintViolation[T]]] =
    GroupBy.apply[String, ConstraintViolation[T]](violations, cv => cv.getPropertyPath.toString)

}