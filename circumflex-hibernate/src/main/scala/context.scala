package circumflex.hibernate

import java.sql.BatchUpdateException
import circumflex.core.HttpResponse
import org.hibernate.exception.ConstraintViolationException


trait TransactionContext {
  def sessionContext: HibernateUtil
}


trait DAO[T] extends TransactionContext {

  def persistentClass: Class[T]

  def createCriteria = sessionContext.createCriteria(persistentClass)

  def findAll = createCriteria.list

  def refresh(entity: T) = sessionContext.currentSession.refresh(entity)

}


class TransactionHelper[R](val sessionContext: HibernateUtil,
                           val actions: HibernateSession => R,
                           var errorHandler: Throwable => R) {

  private var constraintViolationHandlers: Map[String, () => R] = Map()

  def constraintViolation(constraintName: String, handler: =>R): TransactionHelper[R] = {
    constraintViolationHandlers += (constraintName -> (() => handler))
    return this
  }

  def execute(): R = {
    val session = sessionContext.openSession
    session.begin
    try {
      val r = actions(session)
      session.commit
      return r
    } catch {
      case e: ConstraintViolationException => {
        var constraintName = e.getConstraintName
        if (constraintName == null && e.getCause.isInstanceOf[BatchUpdateException]) {
          val sqle = e.getCause.asInstanceOf[BatchUpdateException].getNextException
          constraintName = sessionContext.dialect.buildSQLExceptionConverter().convert(sqle, "", "")
              .asInstanceOf[ConstraintViolationException].getConstraintName
        }
        val r = constraintViolationHandlers.get(constraintName) match {
          case Some(handler) => handler()
          case None => errorHandler(e)
        }
        session.rollback
        return r
      } case e => {
        val r = errorHandler(e)
        session.rollback
        return r
      }
    } finally {
      if (session.isOpen) session.close
    }
  }

}


trait TxHelper extends TransactionContext {

  def tx(actions: HibernateSession => HttpResponse)
        (errorHandler: Throwable => HttpResponse) =
    new TransactionHelper[HttpResponse](sessionContext, actions, errorHandler)

}

