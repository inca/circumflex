package ru.circumflex.core

import java.util.{ResourceBundle, Locale}
import collection.mutable.HashMap

/*!# Circumflex Configuration

This singleton can be used to retrieve Circumflex configuration parameters
throughout your application.

Configuration parameters are read from `cx.properties` available in classpath.

You can also configure your application in runtime, just add your configuration
parameters into `Circumflex` using methods of Scala's `Map` interface.
Note, however, that `Circumflex` singleton is not thread-safe, so it is a best
practice to only set configuration parameters inside some initialization block
which will be executed only once by only one thread.

Note that [Circumflex Maven Plugin](http://circumflex.ru/maven-plugin.html)
also enables you to configure your application at build time using Maven properties
(specified either in application's `pom.xml` or in `settings.xml`) and system
properties (specified in command line). This is very robust production scenario,
because it allows different configurations in different environments without
manual filtering sources and resources.
*/

/**
 * Provides access to Circumflex configuration parameters.
 *
 * Circumflex configuration parameters are read from `cx.properties` which should be
 * available in classpath of your application.
 *
 * You can also set configuration parameters in runtime using the methods of Scala `Map`.
 * Note, however, that `Circumflex` singleton is not thread-safe. It is a best practice to
 * only set configuration parameters inside some initialization block of your application.
 */
object Circumflex extends HashMap[String, Any] {

  /*! The configuration object is initialized by reading `cx.properties`. */
  try {
    val bundle = ResourceBundle.getBundle("cx", Locale.getDefault)
    val keys = bundle.getKeys
    while (keys.hasMoreElements) {
      val k = keys.nextElement
      this(k) = bundle.getString(k)
    }
  } catch {
    case _ => CX_LOG.error("Could not read configuration parameters from cx.properties.")
  }

  /*! Circumflex configuration also offers you a convenient way to configure
  different implementations of components and services, such as configuring
  dialects or connection providers for [Circumflex ORM](http://circumflex.ru/orm.html)
  or request routers for [Circumflex Web Framework](http://circumflex.ru/web.html).

  The logic is pretty simple. Let's say an application or library expects you
  to provide an implementation of some interface, for example, `MyService`, and
  has a default implementation, for example, `DefaultMyService`:

      val mySvc = Circumflex.get("myApp.myService", DefaultMyService)

  Then you can override this implementation by setting configuration parameter
  (`myApp.myService` in our example) to one of the following values:

    * an object itself or it's class, if you run some initialization code before your
    application starts:

        Circumflex("myApp.myService") = new MyServiceImpl
        // or
        Circumflex("myApp.myService") = classOf[MyServiceImpl]

    * class name of your implementation, if you use `cx.properties`:

        myApp.myService=com.myapp.MyServiceImpl

  Note that Scala singletons also work pretty fine as service implementations,
  but you should remember to add a dollar sign (`$`) to the class name.
  For example, if you have following singleton:

      package com.myapp
      object MyServiceImpl extends MyService { ... }

  then set your `myApp.myService` configuration parameter to `com.myapp.MyServiceImpl$`.

  Also note that the instantiation is done using default public class constructors, so
  make sure that the supplied class has one.
  */

  /**
   * Reads a configuration parameter with specified `name` and returns either an object
   * of specified type `C` or, if failed, specified by-name object `default`.
   * If the configuration parameter resolves into `Class` or `String` containing
   * fully-qualified class name, then the object is instantiated. If the supplied
   * class resolves to Scala singleton (`object`), then it's instance is returned.
   *
   * This method caches the result (by overwriting the configuration parameter), so neither
   * `default` nor object instantiation are executed twice.
   */
  def get[C](name: String, default: =>C): C = synchronized {
    val o = newObject(name, default)
    this(name) = o
    return o
  }

  /*! You can also ask `Circumflex` to instantiate a new object instead of getting a cached one
  using `newObject` method:

      val transaction = Circumflex.newObject("myApp.transaction", new DefaultTransaction)

  Note, however, that singletons cannot be instantiated more than once, so if you'll ask
  Circumflex to instantiate a singleton (or provide singleton as default), you'll get the
  same instance on each invokation (just like using the `get` method).

  If configuration parameter already contains an object it will be instantiated again using
  the default constructor (so make sure it has one).
  */

  /**
   * Reads a configuration parameter with specified `name` and returns either new object of
   * specified type `C` or specified by-name object `default`, if failed.
   * If the configuration parameter resolves into `Class` or `String`
   * containing fully-qualified class name, then the object is instantiated. If the supplied
   * class resolves to Scala singleton (`object`), then it's instance is returned.
   */
  def newObject[C](name: String, default: =>C): C = this.get(name) match {
    case Some(obj: C) => instantiateObject(name, obj.asInstanceOf[AnyRef].getClass, default)
    case Some(c: Class[C]) => instantiateObject(name, c, default)
    case Some(s: String) => try {
      // lookup class
      val c = Class.forName(s)
      // cache class so we don't have to look it up again
      this(name) = c
      // try to instantiate an object
      instantiateObject(name, c, default)
    } catch {
      case e: ClassNotFoundException =>
        CX_LOG.error("Could not find class for configuration parameter '" + name + "'.", e)
        default
    }
    case _ => default
  }

  protected def instantiateObject[C](name: String, c: Class[_], default: =>C): C = try {
    // we try to treat a class as Scala singleton first
    c.getField("MODULE$").get(null).asInstanceOf[C]
  } catch {
    case _ => try {
      // if we cannot obtain singleton instance, we stick with plain instantiation
      c.newInstance.asInstanceOf[C]
    } catch {
      case e =>
        // if instantiation fails, we return provided `default`
        CX_LOG.error("Could not instantiate configuration parameter '" + name + "'.", e)
        default
    }
  }

  override def stringPrefix = "cx"
}