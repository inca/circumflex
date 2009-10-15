package circumflex.hibernate

import core.{RouteContext, AbstractCircumflexFilter}
import javax.servlet._
import http.{HttpServletRequest, HttpServletResponse}

import org.slf4j.LoggerFactory
import util.matching.Regex

abstract class HibernateCircumflexFilter extends AbstractCircumflexFilter with TransactionContext {
  val log = LoggerFactory.getLogger("circumflex.hibernate")

  def doFilter(ctx: RouteContext, chain: FilterChain) = {
    transaction(provider.currentSession)(sess => {
      chain.doFilter(ctx.request, ctx.response)
    })(e => {
      log.error("Failed to commit current transaction.", e)
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