package circumflex.hibernate

import circumflex.core.HttpResponse
import javax.validation.ConstraintViolation


trait HibernateHelper extends TransactionContext {

  def tx(actions: HibernateSession => HttpResponse)
        (error: Throwable => HttpResponse) = transaction(actions)(error)

  def validate[T](obj: T): Option[Seq[ConstraintViolation[T]]] = {
    val set = provider.validator.validate(obj)
    if (set.size == 0) None
    else Some(set.toArray.map(_.asInstanceOf[ConstraintViolation[T]]))
  }

}