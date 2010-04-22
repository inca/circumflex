package ru.circumflex.orm

import ru.circumflex.core.{CircumflexContext, AbstractCircumflexFilter}
import ORM._
import javax.servlet.{ServletRequestEvent, ServletRequestListener, FilterChain}

// ## Web Application lifecycle

// It is best practice to demarcate transactions in your web application using
// transaction-per-request design. `TransactionManagementFilter` and
// `TransactionManagementListener` help you with that: they commit current
// transaction after every request.
//
// **Note:** you must ensure that no application code access current transaction
// after the filter or listener is called (or you will quickly run out of connections
// in pool). So make sure that they are executed at very last stage of request
// processing (the `TransactionManagementFilter` should be first in chain).

/**
 * The filter implementation of transaction-per-request lifecycle.
 */
class TransactionManagementFilter extends AbstractCircumflexFilter {

  /**
   * Commit current transaction at the end of request processing cycle and close
   * current connection.
   */
  def doFilter(ctx: CircumflexContext, chain: FilterChain) = {
    chain.doFilter(ctx.request, ctx.response)
    if (transactionManager.hasLiveTransaction ) try {
      tx.commit
      ormLog.debug("Committed current transaction.")
    } catch {
      case e => {
        ormLog.error("An error has occured while trying to commit current transaction.", e)
        tx.rollback
        ormLog.debug("Rolled back current transaction.")
      }
    } finally {
      tx.close
      ormLog.debug("Closed current connection.")
    }
  }
}

/**
 * The request listener implementation of transaction-per-request lifecycle.
 */
class TransactionManagementListener extends ServletRequestListener {
  def requestInitialized(sre: ServletRequestEvent) = {}

  /**
   * Ensure that current transaction is committed and that contextual connection is closed
   * at the end of request processing cycle.
   */
  def requestDestroyed(sre: ServletRequestEvent) =
    if (transactionManager.hasLiveTransaction) try {
      tx.commit
      ormLog.debug("Committed current transaction.")
    } catch {
      case e => {
        ormLog.error("An error has occured while trying to commit current transaction.", e)
        tx.rollback
        ormLog.debug("Rolled back current transaction.")
      }
    } finally {
      tx.close
      ormLog.debug("Closed current connection.")
    }
}
