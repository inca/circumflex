package ru.circumflex.core

import java.util.{ResourceBundle, Locale}
import collection.mutable.HashMap

/*!# Configuration API

This singleton can be used to retrieve Circumflex configuration parameters
throughout your application.

Configuration parameters are read from `cx.properties` which should be available
in classpath.

You can also configure your application in runtime, just add your configuration
parameters into `Circumflex` using methods of Scala's `Map` interface.
Note, however, that `Circumflex` singleton is not thread-safe, so it is a best
practice to only set configuration parameters inside some initialization block
which will be executed only once by only one thread.

[Circumflex Maven Plugin](http://circumflex.ru/maven-plugin.html)
enables you to configure your application at build time using Maven properties
(specified either in application's `pom.xml` or in `settings.xml`) and system
properties (specified in command line). This is very robust production scenario,
because it allows different configurations in different environments without
manual filtering sources and resources.
*/

/**
 * Provides access to Circumflex configuration parameters.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0/circumflex-core/circumflex.scala">circumflex.scala</a>.
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
  We call this mechanism an *instantiation facility*.

  The logic is pretty simple. Let's say an application or library expects you
  to provide an implementation of some interface, for example, `MyService`, and
  has a default implementation, for example, `DefaultMyService`:

      cx.instantiate[MyService]("myApp.myService", DefaultMyService)

  Then you can override this implementation by setting the configuration parameter
  (`myApp.myService` in our example) to one of the following values:

    * the class of the desired object, if you run some initialization code:

        cx("myApp.myService") = classOf[MyServiceImpl]

    * class name of your implementation, if you use `cx.properties`:

        myApp.myService=com.myapp.MyServiceImpl

  Scala singletons might also work pretty fine as service implementations,
  but you should remember to add a dollar sign (`$`) to the class name.

  For example, if you have following singleton:

      package com.myapp
      object MyServiceImpl extends MyService { ... }

  then set your `myApp.myService` configuration parameter to `com.myapp.MyServiceImpl$`.
  Note that singletons cannot be instantiated more than once, so you'll get the same
  instance each time.

  Also note that the instantiation is done using default public class constructors, so
  make sure that the supplied class has one.
  */

  def instantiate[C](name: String, default: =>C): C = this.get(name) match {
    case Some(c: Class[C]) => instantiateObject(name, c, default)
    case Some(s: String) => try {
      instantiateObject(name, Class.forName(s), default)
    } catch {
      case e: ClassNotFoundException =>
        CX_LOG.error("Could not find class for configuration parameter '" + name + "'.", e)
        default
    }
    case _ => default
  }

  def instantiate[C](name: String): C = instantiate[C](name,
    throw new CircumflexException("Could not instantiate configuration parameter '" + name + "'."))

  /*! Internally the instantiation is performed by the `instantiateObject` method. */
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