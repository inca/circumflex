package pro.circumflex
package core

import collection.mutable.HashMap

/*!# Circumflex Configuration

The `Circumflex` singleton is used to retrieve Circumflex configuration parameters
throughout your application. It is essentially a `mutable.HashMap` with
`KeyValueCoercion`.

Configuration parameters are read from `cx.properties` which should be available
in classpath.

You can also configure your application in runtime, just add your configuration
parameters into `Circumflex` using the methods of Scala's `Map` interface.
Note, however, that `Circumflex` singleton is not thread-safe, so it is a best
practice to only set configuration parameters inside some initialization block
which will be executed only once by only one thread (e.g. `ServletContextListener`
in web application).

Circumflex Maven Plugin enables you to configure your application at build
time using Maven properties (specified either in application's `pom.xml` or
in `settings.xml`) and system properties (specified in command line).
This is a very robust production scenario, because it allows different
configurations in different environments and lets you avoid manual sources and
resources filtering.
*/

object Circumflex extends HashMap[String, Any] with KeyValueCoercion {
  override def stringPrefix = "cx"

  try {

  }
}
