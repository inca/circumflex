/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

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
