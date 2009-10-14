package circumflex.hibernate

import circumflex.core.HttpResponse


trait TransactionContext {

  def sessionContext: HibernateUtil

  def transaction[R](actions: HibernateSession => R)(error: Throwable => R): R = {
    val session = sessionContext.openSession
    session.begin
    try {
      val r = actions(session)
      session.commit
      r
    } catch {
      case e => {
        val r = error(e)
        session.rollback
        r
      }
    } finally {
      if (session.isOpen) session.close
    }
  }

}


trait DAO[T] extends TransactionContext {

  def persistentClass: Class[T]

  def createCriteria = sessionContext.createCriteria(persistentClass)

  def findAll = createCriteria.list

  def refresh(entity: T) = sessionContext.currentSession.refresh(entity)

}


trait TransactionHelper extends TransactionContext {

  def tx(actions: HibernateSession => HttpResponse)(error: Throwable => HttpResponse): HttpResponse =
    transaction(actions)(error)

}