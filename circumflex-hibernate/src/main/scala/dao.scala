package circumflex.hibernate

trait BaseDAO[T] {
  def persistentClass: Class[T]
  def sessionContext: HibernateUtil
  def findAll = sessionContext.createCriteria(persistentClass).list
}