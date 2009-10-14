package circumflex.hibernate


import org.hibernate.HibernateException

trait DAO[T] {

  def persistentClass: Class[T]

  def sessionContext: HibernateUtil

  def createCriteria = sessionContext.createCriteria(persistentClass)

  def findAll = createCriteria.list

  def refresh(entity: T) = sessionContext.currentSession.refresh(entity)

  def transaction[R](actions: HibernateSession => R)(error: HibernateException => R): R = {
    val session = sessionContext.openSession
    var blockResult: R = null
    session.begin
    try {
      blockResult = actions(session)
      session.commit
    } catch {
      case e: HibernateException => {
        blockResult = error(e)
        session.rollback
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