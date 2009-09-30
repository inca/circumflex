package circumflex.hibernate

import java.io.Serializable
import java.sql.Connection
import org.hibernate._
import org.hibernate.cfg.AnnotationConfiguration
class HibernateSession(val session: Session) {
  // mail lifecycle methods
  def begin = session.getTransaction.begin
  def commit = session.getTransaction.commit
  def rollback = session.getTransaction.rollback
  def close = session.close
  // criteria creation methods
  def createCriteria[T](persistentClass: Class[T]): HibernateCriteria[T] =
    new HibernateCriteria[T](session.createCriteria(persistentClass))
  def createCriteria[T](persistentClass: Class[T], alias: String): HibernateCriteria[T] =
    new HibernateCriteria[T](session.createCriteria(persistentClass, alias))
  def createCriteria[T](entityName: String): HibernateCriteria[T] =
    new HibernateCriteria[T](session.createCriteria(entityName))
  def createCriteria[T](entityName: String, alias: String): HibernateCriteria[T] =
    new HibernateCriteria[T](session.createCriteria(entityName, alias))
  // query creation methods
  def createFilter(collection: Object, queryString: String) = session.createFilter(collection, queryString)
  def createQuery(queryString: String) = session.createQuery(queryString)
  def createSQLQuery(queryString: String) = session.createSQLQuery(queryString)
  def getNamedQuery(queryName: String) = session.getNamedQuery(queryName)
  // entity manipulation methods
  def get[E, I <: Serializable](persistentClass: Class[E], id: I): Option[E] = {
    val obj = session.get(persistentClass, id)
    if (obj == null) None
    else Some(obj.asInstanceOf[E])
  }
  def get[E, I <: Serializable](persistentClass: Class[E], id: I, lockMode: LockMode): Option[E] ={
    val obj = session.get(persistentClass, id, lockMode)
    if (obj == null) None
    else Some(obj.asInstanceOf[E])
  }
  def get[E, I <: Serializable](entityName: String, id: I): Option[E] = {
    val obj = session.get(entityName, id)
    if (obj == null) None
    else Some(obj.asInstanceOf[E])
  }
  def get[E, I <: Serializable](entityName: String, id: I, lockMode: LockMode): Option[E] = {
    val obj = session.get(entityName, id, lockMode)
    if (obj == null) None
    else Some(obj.asInstanceOf[E])
  }
  def load[E, I <: Serializable](persistentClass: Class[E], id: I): E =
    session.load(persistentClass, id).asInstanceOf[E]
  def load[E, I <: Serializable](persistentClass: Class[E], id: I, lockMode: LockMode): E =
    session.load(persistentClass, id, lockMode).asInstanceOf[E]
  def load[E, I <: Serializable](entityName: String, id: I): E =
    session.load(entityName, id).asInstanceOf[E]
  def load[E, I <: Serializable](entityName: String, id: I, lockMode: LockMode): E =
    session.load(entityName, id, lockMode).asInstanceOf[E]
  def delete(obj: Object) = session.delete(obj)
  def delete(entityName: String, obj: Object) = session.delete(entityName, obj)
  def lock(obj: Object, lockMode: LockMode) = session.lock(obj, lockMode)
  def lock(entityName: String, obj: Object, lockMode: LockMode) = session.lock(entityName, obj, lockMode)
  def merge[T](obj: T):T = session.merge(obj).asInstanceOf[T]
  def merge[T](entityName: String, obj: T):T = session.merge(entityName, obj).asInstanceOf[T]
  def persist(obj: Object) = session.persist(obj)
  def persist(entityName: String, obj: Object) = session.persist(entityName, obj)
  def refresh(obj: Object) = session.refresh(obj)
  def refresh(obj: Object, lockMode: LockMode) = session.refresh(obj, lockMode)
  def replicate(obj: Object, mode: ReplicationMode) = session.replicate(obj, mode)
  def replicate(entityName: String, obj: Object, mode: ReplicationMode) = session.replicate(entityName, obj, mode)
  def save(obj: Object) = session.save(obj)
  def save(entityName: String, obj: Object) = session.save(entityName, obj)
  def saveOrUpdate(obj: Object) = session.saveOrUpdate(obj)
  def saveOrUpdate(entityName: String, obj: Object) = session.saveOrUpdate(entityName, obj)
  def update(obj: Object) = session.update(obj)
  def update(entityName: String, obj: Object) = session.update(entityName, obj)
  // modes
  def getCacheMode = session.getCacheMode
  def setCacheMode(cacheMode: CacheMode) = session.setCacheMode(cacheMode)
  def getFlushMode = session.getFlushMode
  def setFlushMode(flushMode: FlushMode) = session.setFlushMode(flushMode)
  def setReadOnly(obj: Object, readOnly: Boolean) = session.setReadOnly(obj, readOnly)
  // others
  def connection = session.connection
  def disconnect = session.disconnect
  def reconnect(connection: Connection) = session.reconnect(connection)
  def isConnected = session.isConnected
  def isOpen = session.isOpen
  def isDirty = session.isDirty
  def cancelQuery = session.cancelQuery
  def clear = session.clear
  def contains(o: Object) = session.contains(o)
  def evict(o: Object) = session.evict(o)
  def flush = session.flush
  def transaction = session.getTransaction
  def statistics = session.getStatistics
  def sessionFactory = session.getSessionFactory

}

