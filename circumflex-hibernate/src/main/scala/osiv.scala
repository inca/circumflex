package circumflex.hibernate

import core.AbstractFilter
import javax.servlet._
import http.{HttpServletRequest, HttpServletResponse}

import util.matching.Regex

abstract class HibernateFilter extends AbstractFilter {

  var staticRegex: Regex = null;

  override def init(filterConfig: FilterConfig) = {
    staticRegex = params.get("staticRegex") match {
      case Some(sr) => sr.r
      case _ => "/static/.*".r
    }
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
    try {
      chain.doFilter(request, response)
      session.commit
    } catch {
      case _ => {
        // TODO log excetion
        session.rollback
      }
    } finally {
      if (session.isOpen)
        session.close
    }
  }

}

class HibernateCoreFilter extends HibernateFilter {
  def hibernateUtil = HUtil
}

class HibernateAnnotationsFilter extends HibernateFilter {
  def hibernateUtil = HAUtil
}