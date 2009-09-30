package circumflex.hibernate

import org.hibernate.cfg.AnnotationConfiguration
import org.hibernate.criterion.{Projection, Order, Criterion}
import org.hibernate.{LockMode, FetchMode, Criteria, Session}
trait HibernateUtil {



}

class HibernateSession(val session: Session) {

  def begin = session.getTransaction.begin
  def commit = session.getTransaction.commit
  def rollback = session.getTransaction.rollback
  def close = session.close

}

class HibernateCriteria[T](val criteria: Criteria) {

  def add(criterion: Criterion) = {
    criteria.add(criterion)
    this
  }

  def addOrder(order: Order) = {
    criteria.addOrder(order)
    this
  }

  def setProjection(projection: Projection) = {
    criteria.setProjection(projection)
    this
  }

  def setFetchMode(associationPath: String, mode: FetchMode) = {
    criteria.setFetchMode(associationPath, mode)
    this
  }

  def setLockMode(mode: LockMode): HibernateCriteria[T] = {
    criteria.setLockMode(mode)
    this
  }

  def setLockMode(alias: String, mode: LockMode): HibernateCriteria[T] = {
    criteria.setLockMode(alias, mode)
    this
  }

  
    

}