package circumflex.hibernate

import circumflex.core.HttpResponse


trait TransactionContext {

  def provider: HibernateConfigurationProvider

  def transaction[R](actions: HibernateSession => R)
                    (errorHandler: Throwable => R): R = {
    val session = provider.openSession
    session.begin
    try {
      val r = actions(session)
      session.commit
      return r
    } catch {
      case e => {
        val r = errorHandler(e)
        session.rollback
        return r
      }
    } finally {
      if (session.isOpen) session.close
    }
  }

}


trait DAO[T] extends TransactionContext {

  def persistentClass: Class[T]

  def createCriteria = provider.createCriteria(persistentClass)

  def findAll = createCriteria.list

  def refresh(entity: T) = provider.currentSession.refresh(entity)

}
