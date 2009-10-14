package circumflex.hibernate

import circumflex.core.HttpResponse


trait HibernateHelper extends TransactionContext {

  def tx(actions: HibernateSession => HttpResponse)
        (error: Throwable => HttpResponse) = transaction(actions)(error)

  def validate[T](obj: T) = provider.validator.validate(obj)

}