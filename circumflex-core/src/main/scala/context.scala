package ru.circumflex.core

import scala.collection.mutable.HashMap

/*!# Context

Context is a thread-local container which allows you to share objects (also known as
context variables) within one logical scope.

Such logical scope could be anything: database transaction, HTTP request, user
session within GUI form, etc. Within this scope you can obtain current context by
calling `Context.get` method (or using `ctx` method of package `ru.circumflex.core`).

Most Circumflex components depend on context and, therefore, can only be run
inside context-aware code. Application is responsible for maintaining context
lifecycle. For example, [Circumflex Web Framework](http://circumflex.ru/web.html)
takes care of context initialization and finalization inside `CircumflexFilter`.
See [Context Lifecycle](#lifecycle) for more information.
*/

/**
 * Provides simple thread-local storage for organizing an execution context of
 * an application. The companion object, `Context`, is used to retrieve a context
 * bound to current thread (a.k.a. current context) as well as to initialize and
 * destroy current context.
 */
class Context extends HashMap[String, Any] {
  override def stringPrefix = "ctx"
}

/*!# Context Lifecycle {#lifecycle}

In order to maintain context scope an application should properly initialize
and destroy contexts. It is done by using `Context.init` and `Context.destroy`
methods.
*/

/**
 * Singleton which is used to initialize, retrieve and destroy current contexts.
 *
 * Circumflex Context API also provides mechanisms to subscribe to context
 * initialization and finalization events. Note that these methods are not thread-safe,
 * so you can only add subscriptions inside some initialization block of your
 * application.
 */
object Context {

  /*! We use thread-local storage so that each thread can get it's own instance
  of context.
  */

  protected val threadLocal = new ThreadLocal[Context]

  /*! You can also add event listeners which will be executed after the context is
  initialized or before the context is destroyed.
  */
  protected var initListeners: Seq[Context => Unit] = Nil
  protected var destroyListeners: Seq[Context => Unit] = Nil

  /**
   * Subscribes specified `listener` to context initialization event.
   */
  def addInitListener(listener: Context => Unit): Unit =
    initListeners ++= List(listener)
  /**
   * Subscribes specified `listener` to context finalization event.
   */
  def addDestroyListener(listener: Context => Unit): Unit =
    destroyListeners ++= List(listener)

  /*! Context is initialized when it is first accessed via `Context.get` method.
  You can override default context implementation by setting `cx.context`
  configuration parameter.
  */

  /**
   * Returns an instance of the `Context` class bound to current thread (a.k.a. current context).
   * Performs context initialization if it hasn't been initialized before.
   */
  def get(): Context = {
    if (!live_?) init()
    return threadLocal.get
  }

  /**
   * Indicates if the current context has been initialized.
   */
  def live_?(): Boolean = threadLocal.get != null

  /**
   * Performs current context initialization. New instance of `Context` is created
   * using either the class provided by `cx.context` configuration parameter or
   * the default implementation, the `Context` class. After initialization each
   * listener subscribed to context initialization event is executed.
   */
  def init(): Unit = {
    threadLocal.set(Circumflex.newObject("cx.context", new Context))
    initListeners.foreach(l => l.apply(get()))
  }

  /**
   * Destroys current context.
   * After that each listener subscribed to context finalization event is executed.
   */
  def destroy(): Unit = {
    if (!live_?) return
    destroyListeners.foreach(l => l.apply(get()))
    threadLocal.set(null)
  }

}

/*!# Context DSL

Circumflex enables you to use Scala `Symbol` to access and set context variables in a DSL
fashion.

Following syntaxes are available for accessing context variables:

    'key.apply[T]                // T
    'key.get[T]                  // Option[T]
    'key.getOrElse(default: T)   // T

Following syntaxes are available for setting context variables:

    'key := value
    'key.update(value)

The implicit conversions from `Symbol` into `ContextVarHelper` are available in the
`ru.circumflex.core` package.
*/
class ContextVarHelper(val key: Symbol) {
  def apply[T](): T = ctx.apply(key).asInstanceOf[T]
  def get[T](): Option[T] = ctx.get(key).asInstanceOf[Option[T]]
  def getOrElse[T](default: T): T = ctx.getOrElse(key, default).asInstanceOf[T]
  def update(value: Any): Unit = ctx.update(key, value)
  def :=(value: Any): Unit = update(value)
}