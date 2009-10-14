package circumflex.hibernate


import org.hibernate.HibernateException

trait DAO[T] {

  def persistentClass: Class[T]

  def sessionContext: HibernateUtil

  def createCriteria = sessionContext.createCriteria(persistentClass)

  def findAll = createCriteria.list

  def refresh(entity: T) = sessionContext.currentSession.refresh(entity)

  def transaction(actions: HibernateSession => Unit)(error: HibernateException => Unit) = {
    val session = sessionContext.openSession
    session.begin
    try {
      actions(session)
      session.commit
    } catch {
      case e: HibernateException => {
        session.rollback
        error(e)
      }
      case e => {
        session.rollback
        throw e
      }
    } finally {
      if (session.isOpen) session.close
    }
  }
}