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
class Context extends HashMap[String, Any] {
  override def stringPrefix = "ctx"
}

/*!# Context Lifecycle {#lifecycle}

In order to maintain context scope an application should properly initialize
and destroy contexts. It is done by using `Context.init` and `Context.destroy`
methods.

We use thread-local storage so that each thread can get it's own instance
of context.

You can also add event listeners which will be executed after the context is
initialized or before the context is destroyed.

Context is initialized when it is first accessed via `Context.get` method.
You can override default context implementation by setting `cx.context`
configuration parameter.
*/
object Context {

  protected val threadLocal = new ThreadLocal[Context]

  protected var initListeners: Seq[Context => Unit] = Nil
  protected var destroyListeners: Seq[Context => Unit] = Nil
  def addInitListener(l: Context => Unit): Unit =
    initListeners ++= List(l)
  def addDestroyListener(l: Context => Unit): Unit =
    destroyListeners ++= List(l)

  def live_?(): Boolean = threadLocal.get != null

  def get(): Context = {
    if (!live_?) init()
    return threadLocal.get
  }

  def init(): Unit = {
    threadLocal.set(Circumflex.newObject("cx.context", new Context))
    initListeners.foreach(l => l.apply(get()))
  }

  def destroy(): Unit = {
    if (!live_?) return
    destroyListeners.foreach(l => l.apply(get()))
    threadLocal.set(null)
  }

}