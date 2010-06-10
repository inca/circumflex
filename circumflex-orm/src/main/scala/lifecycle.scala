package ru.circumflex.orm

import _root_.ru.circumflex.core.{Circumflex, CircumflexContext, AbstractCircumflexFilter}
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
  def doFilter(ctx: CircumflexContext, chain: FilterChain) =
    transactionManager.executeInContext(tx) { chain.doFilter(ctx.request, ctx.response) }
}

/**
 * The request listener implementation of transaction-per-request lifecycle.
 */
class TransactionManagementListener extends ServletRequestListener {
  def requestInitialized(sre: ServletRequestEvent) = {}
  def requestDestroyed(sre: ServletRequestEvent) = transactionManager.executeInContext(tx){}
}
