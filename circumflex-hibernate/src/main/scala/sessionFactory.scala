package ru.circumflex.hibernate

import java.io.Serializable
import javax.validation.{ValidatorFactory, Validation}
import org.hibernate.cfg.{AnnotationConfiguration, Configuration}
import org.hibernate.dialect.Dialect
import org.hibernate.LockMode

abstract class HibernateProvider(val configuration: Configuration,
                        val validationFactory: ValidatorFactory) {

  def this(conf: Configuration) =
    this(conf, Validation.buildDefaultValidatorFactory)

  val sessionFactory = configuration.buildSessionFactory;
  val dialect = Dialect.getDialect(configuration.getProperties);

  def validator = validationFactory.getValidator

  def currentSession: HibernateSession = new HibernateSession(sessionFactory.getCurrentSession)
  def openSession: HibernateSession = new HibernateSession(sessionFactory.openSession)
  // convenience methods (delegated to current session)
  def createCriteria[T](persistentClass: Class[T]): HibernateCriteria[T] =
    currentSession.createCriteria(persistentClass)
  def createCriteria[T](persistentClass: Class[T], alias: String): HibernateCriteria[T] =
    currentSession.createCriteria(persistentClass, alias)
  def get[E, I <: Serializable](persistentClass: Class[E], id: I): Option[E] =
    currentSession.get(persistentClass, id)
  def get[E, I <: Serializable](persistentClass: Class[E], id: I, lockMode: LockMode): Option[E] =
    currentSession.get(persistentClass, id, lockMode)
  def refresh(obj: Any) = currentSession.refresh(obj)
  def refresh(obj: Any, lockMode: LockMode) = currentSession.refresh(obj, lockMode)
}

object HUtil extends HibernateProvider(new Configuration().configure)

object HAUtil extends HibernateProvider(new AnnotationConfiguration().configure)
