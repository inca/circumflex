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

import core.{CircumflexContext, AbstractCircumflexFilter}
import javax.servlet._
import org.slf4j.LoggerFactory

abstract class HibernateCircumflexFilter extends AbstractCircumflexFilter with TransactionContext {
  override protected val log = LoggerFactory.getLogger("ru.circumflex.hibernate")

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