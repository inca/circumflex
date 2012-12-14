package pro.savant.circumflex
package core

import collection.mutable.{ListBuffer, HashMap}

/*!# Context API

Context is a thread-local container which allows you to share
objects (also known as context variables) within one logical scope
bound to the executing thread. Such logical scope could be anything:
 database transaction, HTTP request, user session within GUI form, etc.

Within this scope you can obtain current context by calling
`Context.get` method (or using `ctx` method of package
`pro.savant.circumflex.core`).

Most Circumflex components depend on context and, therefore,
can only be run inside context-aware code. Your application is
responsible for maintaining context lifecycle.

For example, Circumflex Web Framework takes care of context
initialization and finalization inside `CircumflexFilter` (see [[web/src/main/scala/filter.scala]]).

Circumflex context is based on `KeyValueCoercion`
(see [[/core/src/main/scala/kvc.scala]]).

# Context Lifecycle

In order to maintain context scope an application should
properly initialize and destroy contexts.

It is done by using `Context.init` and `Context.destroy` methods.

## Events

You can add event listeners which will be executed after the context
is initialized or before the context is destroyed.

Context is initialized either explicitly or when it is first
accessed via the `Context.get` method.
*/
class Context extends HashMap[String, Any] with KeyValueCoercion {
  override def stringPrefix = "ctx"
}

object Context {

  protected val threadLocal = new ThreadLocal[Context]

  protected val _initListeners = new ListBuffer[Context => Unit]
  def initListeners = _initListeners
  def addInitListener(listener: Context => Unit) {
    _initListeners += listener
  }
  def insertInitListener(index: Int, listener: Context => Unit) {
    _initListeners.insert(index, listener)
  }

  protected val _destroyListeners = new ListBuffer[Context => Unit]
  def destroyListeners = _destroyListeners
  def addDestroyListener(listener: Context => Unit) {
    _destroyListeners += listener
  }
  def insertDestroyListener(index: Int, listener: Context => Unit) {
    _destroyListeners.insert(index, listener)
  }

  def get(): Context = {
    if (!isLive) init()
    threadLocal.get
  }

  def isLive: Boolean = threadLocal.get != null

  def init() {
    threadLocal.set(new Context)
    initListeners.foreach(l => l.apply(get()))
  }

  def destroy() {
    if (isLive) {
      destroyListeners.foreach(_.apply(get()))
      threadLocal.set(null)
    }
  }

  /*! The `executeInNew` method defines the boundaries of the context.

  It wraps the specified `block` inside a context block, which
  creates and initializes a new instance of Context at the beginning
  and performs the finalization at the end.

  The most typical scenario of using this method is
  to perform transaction demarcations in Circumflex ORM.
  */
  def executeInNew[A](block: Context => A): A = {
    val previousCtx: Option[Context] =
      if (isLive) Some(get()) else None
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

Circumflex enables you to use Scala `Symbol` to access
and set context variables in a DSL fashion.

Following syntaxes is used for setting context variables:

``` {.scala}
'key := value
```

In order to be able to use such DSL in your application you should
import the implicit conversion methods from
the `pro.savant.circumflex.core` package.
*/
class ContextVarHelper(val sym: Symbol) {
  def :=(value: Any) {
    ctx.update(sym.name, value)
  }
}