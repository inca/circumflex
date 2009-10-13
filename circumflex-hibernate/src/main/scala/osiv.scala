package circumflex.hibernate

import core.AbstractFilter
import javax.servlet._
import http.{HttpServletRequest, HttpServletResponse}

import org.slf4j.LoggerFactory
import util.matching.Regex

abstract class HibernateFilter extends AbstractFilter {
  val log = LoggerFactory.getLogger("circumflex.hibernate")
  var staticRegex: Regex = null;

  override def init(filterConfig: FilterConfig) = {
    staticRegex = initParams.get("staticRegex") match {
      case Some(sr) => sr.r
      case _ => "/static/.*".r
    }
    log.debug("HibernateFilter instance successfully configured.");
    log.debug("staticRegex is {}", staticRegex);
  }

  def hibernateUtil: HibernateUtil

  def doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain): Unit = {
    val req = request.asInstanceOf[HttpServletRequest]
    val res = response.asInstanceOf[HttpServletResponse]
    if (staticRegex.pattern.matcher(req.getRequestURI).matches) {
      chain.doFilter(req,res)
      return
    }
    val session = hibernateUtil.currentSession
    session.begin
    log.trace("Current transaction: BEGIN")
    try {
      chain.doFilter(request, response)
      session.commit
      log.trace("Current transaction: COMMIT")
    } catch {
      case e => {
        log.trace("Current transaction: ERROR", e)
        session.rollback
        log.trace("Current transaction: ROLLBACK")
      }
    } finally {
      if (session.isOpen)
        session.close
    }
  }

}

class HibernateCoreFilter extends HibernateFilter {
  def hibernateUtil = HUtil
  log.info("Initializing Hibernate Filter, Core version.")
}

class HibernateAnnotationsFilter extends HibernateFilter {
  def hibernateUtil = HAUtil
  log.info("Initializing Hibernate Filter, Annotations version.")
}