package pro.circumflex.core

import collection.mutable.HashMap

/*!# Context API

Context is essentially a simple key-value storage bound to currently
executing thread (with `ThreadLocal`) which allows sharing data across
different objects, components and method calls within a single logical scope.

Context API is heavily used in both Circumflex modules and applications built
with Circumflex. The most notable usages are:

  * passing data from routers to template engines (Web Framework);
  * establishing transaction demarcation (ORM);
  * seamless transaction-per-request pattern (Web Framework + ORM);
  * executing database transactions in futures, actors and other
    thread-sensitive places.

*/
class Context extends HashMap[String, Any] with KeyValueCoercion {
  override def stringPrefix = "ctx"
}

/*!## Data reading and writing

Following DSL is used for accessing data from current context:

  * `'key := value` for updating values;
  * `'key.get` for reading values.

In order to use these DSL methods you should have following import
statement in place:

``` {.scala}
import pro.circumflex.core._
```

Note that the DSL is achieved via implicit method.
*/
class ContextVarHelper(val sym: Symbol) {
  def get = ctx.get(sym.name)
  def :=(value: Any) {
    ctx.update(sym.name, value)
  }
}

/*!## Context lifecycle

Apart from being a convenient way to pass the data along the thread's flow,
Circumflex Context API also provides the semantics of simple logical
transactions.

This is achieved by wrapping the block of execution in `transaction { }`
(imported from package object `core`). All the code inside this block
share the same context, which can be accessed with `ctx` method (treat
it as a simple mutable `HashMap[String, Any]`.

Furthermore, an application can attach the listeners on context initialization
and finalization events. Such events are handled by `ContextManager` singleton.
*/
object ContextManager {

  protected val _tl = new ThreadLocal[Context]

  def isLive: Boolean = _tl.get != null

  def get: Context = {
    if (!isLive) init()
    _tl.get
  }

  protected def init() {
    _tl.set(new Context)
  }

  protected def destroy() {
    if (isLive) {
      _tl.set(null)
    }
  }

  def transaction[A](ops: => A): A = {
    val prev = _tl.get
    try {
      init()
      ops
    } finally {
      destroy()
      _tl.set(prev)
    }
  }

}

