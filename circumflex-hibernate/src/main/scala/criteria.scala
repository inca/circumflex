package circumflex.hibernate

import org.hibernate._
import org.hibernate.criterion.{Criterion, Order, Projection}
import org.hibernate.transform.ResultTransformer

class HibernateCriteria[T](val criteria: Criteria) {

  def add(criterion: Criterion): HibernateCriteria[T] = {
    criteria.add(criterion)
    this
  }

  def addOrder(order: Order): HibernateCriteria[T] = {
    criteria.addOrder(order)
    this
  }

  def setProjection(projection: Projection): HibernateCriteria[T] = {
    criteria.setProjection(projection)
    this
  }

  def setFetchMode(associationPath: String, mode: FetchMode): HibernateCriteria[T] = {
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

  def createAlias(associationPath: String, alias: String): HibernateCriteria[T] = {
    criteria.createAlias(associationPath, alias)
    this
  }

  def createAlias(associationPath: String, alias: String, joinType: Int): HibernateCriteria[T] = {
    criteria.createAlias(associationPath, alias, joinType)
    this
  }

  def createCriteria(associationPath: String): HibernateCriteria[T] =
    new HibernateCriteria[T](criteria.createCriteria(associationPath))
  
  def createCriteria(associationPath: String, joinType: Int): HibernateCriteria[T] =
    new HibernateCriteria[T](criteria.createCriteria(associationPath, joinType))

  def createCriteria(associationPath: String, alias: String): HibernateCriteria[T] =
    new HibernateCriteria[T](criteria.createCriteria(associationPath, alias))

  def createCriteria(associationPath: String, alias: String, joinType: Int): HibernateCriteria[T] =
    new HibernateCriteria[T](criteria.createCriteria(associationPath, alias, joinType))

  def setResultTransformer(resultTransformer: ResultTransformer): HibernateCriteria[T] = {
    criteria.setResultTransformer(resultTransformer)
    this
  }

  def setMaxResults(maxResults: Int): HibernateCriteria[T] = {
    criteria.setMaxResults(maxResults)
    this
  }

  def setFirstResult(firstResult: Int): HibernateCriteria[T] = {
    criteria.setFirstResult(firstResult)
    this
  }

  def setFetchSize(fetchSize: Int): HibernateCriteria[T] = {
    criteria.setFetchSize(fetchSize)
    this
  }

  def setTimeout(timeout: Int): HibernateCriteria[T] = {
    criteria.setTimeout(timeout)
    this
  }

  def setCacheable(cacheable: Boolean): HibernateCriteria[T] = {
    criteria.setCacheable(cacheable)
    this
  }

  def setCacheRegion(cacheRegion: String): HibernateCriteria[T] = {
    criteria.setCacheRegion(cacheRegion)
    this
  }

  def setComment(comment: String): HibernateCriteria[T] = {
    criteria.setComment(comment)
    this
  }

  def setFlushMode(flushMode: FlushMode): HibernateCriteria[T] = {
    criteria.setFlushMode(flushMode)
    this
  }

  def setCacheMode(cacheMode: CacheMode): HibernateCriteria[T] = {
    criteria.setCacheMode(cacheMode)
    this
  }

  def list: Seq[T] = criteria.list.toArray.map(_.asInstanceOf[T])

  def uniqueResult: Option[T] = {
    val obj = criteria.uniqueResult
    if (obj == null) None
    else Some(obj.asInstanceOf[T])
  }

}