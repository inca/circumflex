package circumflex.hibernate

import circumflex.core.HttpResponse
import java.io.Serializable
import org.hibernate.criterion.Restrictions


trait TransactionContext {

  def provider: HibernateConfigurationProvider

  def transaction[R](session: HibernateSession)
                    (actions: HibernateSession => R)
                    (errorHandler: Throwable => R): R = {
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

  def transaction[R](actions: HibernateSession => R)
                    (errorHandler: Throwable => R): R =
    transaction(provider.openSession)(actions)(errorHandler)

}


trait DAO[T, ID <: Serializable] extends TransactionContext {

  def persistentClass: Class[T]

  def createCriteria = provider.createCriteria(persistentClass)

  def findAll = createCriteria.list

  def refresh(entity: T) = provider.refresh(entity)

  def findById(id: ID): Option[T] = provider.get(persistentClass, id)

  def createAndApply(params: Map[String, Object]): Option[T] =
    apply(persistentClass.getConstructor().newInstance().asInstanceOf[T], params)

  def getAndApply(id: ID, params: Map[String, Object]): Option[T] = findById(id) match {
    case Some(entity) => apply(entity, params)
    case _ => None
  }

  def apply(entity: T, params: Map[String, Object]): Option[T] = Some(entity)

}
