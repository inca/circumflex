package ru.circumflex
package core

import scala.collection.mutable.HashMap
import java.util.Date

/*!# Context API

Context is a thread-local container which allows you to share objects (also known as
context variables) within one logical scope.

Such logical scope could be anything: database transaction, HTTP request, user
session within GUI form, etc. Within this scope you can obtain current context by
calling `Context.get` method (or using `ctx` method of package `ru.circumflex.core`).

Most Circumflex components depend on context and, therefore, can only be run
inside context-aware code. Application is responsible for maintaining context
lifecycle. For example, [Circumflex Web Framework](http://circumflex.ru/projects/web/index.html)
takes care of context initialization and finalization inside `CircumflexFilter`.
See [Context Lifecycle](#lifecycle) for more information.

Circumflex context also extends `UntypedContainer` so you may access instantiation
fancies as well as coercing parameters to expected types.

# Context Lifecycle {#lifecycle}

In order to maintain context scope an application should properly initialize
and destroy contexts. It is done by using `Context.init` and `Context.destroy`
methods.

You can also add event listeners which will be executed after the context is
initialized or before the context is destroyed.

Context is initialized when it is first accessed via `Context.get` method.
You can override default context implementation by setting `cx.context`
configuration parameter.
*/
class Context extends HashMap[String, Any] with UntypedContainer {
  override def stringPrefix = "ctx"
}

object Context {

  // We use thread-local storage so that each thread can get it's own instance of context.

  protected val threadLocal = new ThreadLocal[Context]

  protected var initListeners: Seq[Context => Unit] = Nil
  protected var destroyListeners: Seq[Context => Unit] = Nil

  def addInitListener(listener: Context => Unit) {
    initListeners ++= List(listener)
  }

  def addDestroyListener(listener: Context => Unit) {
    destroyListeners ++= List(listener)
  }

  def get(): Context = {
    if (!isLive) init()
    threadLocal.get
  }

  def isLive: Boolean = threadLocal.get != null

  def init() = {
    threadLocal.set(cx.instantiate[Context]("cx.context", new Context))
    initListeners.foreach(l => l.apply(get()))
  }

  def destroy() = if (isLive) {
    destroyListeners.foreach(_.apply(get()))
    threadLocal.set(null)
  }

  def executeInNew[A](block: Context => A): A = {
    val previousCtx: Option[Context] = if (isLive) Some(get()) else None
    try {
      Context.init()
      val _ctx = get()
      block(_ctx)
    } finally {
      Context.destroy()
      previousCtx.map(threadLocal.set(_))
    }
  }

}

/*!# Context DSL

Circumflex enables you to use Scala `Symbol` to access and set context variables in a DSL
fashion.

Following syntaxes are available for accessing context variables:

    'key.apply[T]                // T                                           {.scala}
    'key.get[T]                  // Option[T]
    'key.getOrElse(default: T)   // T

Following syntaxes are available for setting context variables:

    'key := value                                                               {.scala}
    'key.update(value)

The implicit conversions from `Symbol` into `ContextVarHelper` are available in the
`ru.circumflex.core` package.
*/
class ContextVarHelper(val sym: Symbol) {
  val key = sym.name
  def apply[T]: T = ctx.as[T](key)
  def get[T]: Option[T] = ctx.getAs[T](key)
  def getOrElse[T >: Any](default: T): T = ctx.getOrElse[T](key, default)
  def getString(key: String): String = getOrElse(key, "").toString
  def getString: String = ctx.getString(key)
  def getBoolean: Boolean = ctx.getBoolean(key)
  def getInt: Int = ctx.getInt(key)
  def getLong: Long = ctx.getLong(key)
  def getDouble: Double = ctx.getDouble(key)
  def getDate(pattern: String): Date = ctx.getDate(key, pattern)

  def update(value: Any) {
    ctx.update(key, value)
  }
  def :=(value: Any) {
    update(value)
  }

}