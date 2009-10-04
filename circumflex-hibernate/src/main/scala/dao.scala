package circumflex.hibernate

trait DAO[T] {
  def persistentClass: Class[T]
  def sessionContext: HibernateUtil
  def createCriteria = sessionContext.createCriteria(persistentClass)
  def findAll = createCriteria.list
  def refresh(entity: T) = sessionContext.currentSession.refresh(entity)
  def save(entity: T) = sessionContext.currentSession.save(entity)
  def saveOrUpdate(entity: T) = sessionContext.currentSession.saveOrUpdate(entity)
  def update(entity: T) = sessionContext.currentSession.update(entity)
  def delete(entity: T) = sessionContext.currentSession.delete(entity)
}