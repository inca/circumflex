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

package ru.circumflex.orm

import ru.circumflex.core.{CircumflexContext, AbstractCircumflexFilter}
import org.slf4j.LoggerFactory
import ORM._
import javax.servlet.{ServletRequestEvent, ServletRequestListener, FilterChain}

/**
 * Ensures that current transaction is commited and that contextual connection is closed
 * at the end of request processing cycle.
 * This filter should be the first in chain.
 */
class TransactionManagementFilter extends AbstractCircumflexFilter {
  override protected val log = LoggerFactory.getLogger("ru.circumflex.orm")

  /**
   * Commits current transaction at the end of request processing cycle and closes current connection.
   */
  def doFilter(ctx: CircumflexContext, chain: FilterChain) = {
    chain.doFilter(ctx.request, ctx.response)
    if (transactionManager.hasLiveTransaction ) try {
      tx.commit
      log.debug("Committed current transaction.")
    } catch {
      case e => {
        log.error("An error has occured while trying to commit current transaction.", e)
        tx.rollback
        log.debug("Rolled back current transaction.")
      }
    } finally {
      tx.close
      log.debug("Closed current connection.")
    }
  }
}

/**
 * Ensures that current transaction is commited and that contextual connection is closed
 * at the end of request processing cycle.
 */
class TransactionManagementListener extends ServletRequestListener {
  protected val log = LoggerFactory.getLogger("ru.circumflex.orm")

  def requestInitialized(sre: ServletRequestEvent) = {}

  def requestDestroyed(sre: ServletRequestEvent) =
    if (transactionManager.hasLiveTransaction) try {
      tx.commit
      log.debug("Committed current transaction.")
    } catch {
      case e => {
        log.error("An error has occured while trying to commit current transaction.", e)
        tx.rollback
        log.debug("Rolled back current transaction.")
      }
    } finally {
      tx.close
      log.debug("Closed current connection.")
    }
}
