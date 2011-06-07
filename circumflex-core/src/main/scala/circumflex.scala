package ru.circumflex.core

import java.util.{Date, ResourceBundle, Locale}
import java.text.SimpleDateFormat
import collection.mutable.{HashMap, Map}

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

[Circumflex Maven Plugin](http://circumflex.ru/projects/maven-plugin/index.html)
enables you to configure your application at build time using Maven properties
(specified either in application's `pom.xml` or in `settings.xml`) and system
properties (specified in command line). This is very robust production scenario,
because it allows different configurations in different environments without
manual filtering sources and resources.
*/
object Circumflex extends HashMap[String, Any] with UntypedContainer {

  // The configuration object is initialized by reading `cx.properties`.
  try {
    val bundle = ResourceBundle.getBundle(
      "cx", Locale.getDefault, Thread.currentThread.getContextClassLoader)
    val keys = bundle.getKeys
    while (keys.hasMoreElements) {
      val k = keys.nextElement
      this(k) = bundle.getString(k)
    }
  } catch {
    case _ => CX_LOG.error("Could not read configuration parameters from cx.properties.")
  }

  override def stringPrefix = "cx"
}

/*! Several helper methods allow you to obtain parameter precisely in the type you expect:

  * `getAs[T]` returns `Option[T]`;
  * `as[T]` returns `[T]`;
  * `getXXX` returns `XXX` trying to coerce the value to the `XXX` type.

A `ClassCastException` is thrown if the configuration contains the value
with different type than you expect.

Circumflex configuration (and every untyped container) also offers you a convenient
way to configure different implementations of components and services, such as configuring
dialects or connection providers for [Circumflex ORM](http://circumflex.ru/projects/orm/index.html)
or request routers for [Circumflex Web Framework](http://circumflex.ru/projects/web/index.html).
We call this mechanism an _instantiation facility_.

The logic is pretty simple. Let's say an application or library expects you
to provide an implementation of some interface, for example, `MyService`, and
has a default implementation, for example, `DefaultMyService`:

    cx.instantiate[MyService]("myApp.myService", DefaultMyService)      {.scala}

Then you can override this implementation by setting the configuration parameter
(`myApp.myService` in our example) to one of the following values:

  * the class of the desired object, if you run some initialization code:

        cx("myApp.myService") = classOf[MyServiceImpl]                  {.scala}

  * class name of your implementation, if you use `cx.properties`:

        myApp.myService=com.myapp.MyServiceImpl                         {.scala}

Scala singletons might also work pretty fine as service implementations,
but you should remember to add a dollar sign (`$`) to the class name.

For example, if you have following singleton:

    package com.myapp                                                   {.scala}
    object MyServiceImpl extends MyService { ... }

then set your `myApp.myService` configuration parameter to `com.myapp.MyServiceImpl$`.
Note that singletons cannot be instantiated more than once, so you'll get the same
instance each time.

Also note that the instantiation is done using default public class constructors, so
make sure that the supplied class has one.
*/
trait UntypedContainer extends Map[String, Any] {

  def as[C](key: String): C = apply(key).asInstanceOf[C]
  def getAs[C](key: String): Option[C] = get(key).asInstanceOf[Option[C]]
  def getString(key: String): String = getOrElse(key, "").toString
  def getBoolean(key: String): Boolean = getOrElse(key, "false").toString.toBoolean
  def getInt(key: String): Int = getOrElse(key, "0").toString.toInt
  def getLong(key: String): Long = getOrElse(key, "0").toString.toLong
  def getDouble(key: String): Double = getOrElse(key, "0").toString.toDouble
  def getDate(key: String, pattern: String): Date =
    get(key).map(v => new SimpleDateFormat(pattern).parse(v.toString)).getOrElse(new Date)

  def instantiate[C](name: String, default: =>C): C = this.get(name) match {
    case Some(c: Class[_]) => instantiateObject(name, c)
    case Some(s: String) => instantiateObject(name, Class.forName(
      s, true, Thread.currentThread.getContextClassLoader))
    case v => default
  }

  def instantiate[C](name: String): C = instantiate[C](name, throw new CircumflexException(
    "Could not perform instantiation for parameter " + name))

  // Internally the instantiation is performed by the `instantiateObject` method.
  protected def instantiateObject[C](name: String, c: Class[_]): C = try {
    c.getField("MODULE$").get(null).asInstanceOf[C]
  } catch {
    case _ => c.newInstance.asInstanceOf[C]
  }

}
