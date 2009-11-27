package ru.circumflex.hibernate

import core.{CircumflexContext, AbstractCircumflexFilter}
import javax.servlet._
import org.slf4j.LoggerFactory

abstract class HibernateCircumflexFilter extends AbstractCircumflexFilter with TransactionContext {
  val log = LoggerFactory.getLogger("ru.circumflex.hibernate.filter")

  def doFilter(ctx: CircumflexContext, chain: FilterChain) = {
    log.debug("About to wrap a request into a contextual transaction.")
    transaction(provider.currentSession)(sess => {
      chain.doFilter(ctx.request, ctx.response)
      log.debug("About to commit a contextual transaction.")
    })(e => {
      log.error("Failed to commit current transaction.", e)
      log.debug("About to rollback a contextual transaction.")
    })
  }

}

class HibernateCoreFilter extends HibernateCircumflexFilter {
  def provider = HUtil
  log.info("Initializing Hibernate Circumflex Filter, Core version.")
}

class HibernateAnnotationsFilter extends HibernateCircumflexFilter {
  def provider = HAUtil
  log.info("Initializing Hibernate Circumflex Filter, Annotations version.")
}