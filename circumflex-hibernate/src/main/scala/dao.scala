package circumflex.hibernate


import org.hibernate.HibernateException

trait DAO[T] {

  def persistentClass: Class[T]

  def sessionContext: HibernateUtil

  def createCriteria = sessionContext.createCriteria(persistentClass)

  def findAll = createCriteria.list

  def refresh(entity: T) = sessionContext.currentSession.refresh(entity)

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