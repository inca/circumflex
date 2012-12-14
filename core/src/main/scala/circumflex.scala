package pro.savant.circumflex
package core

import collection.mutable.{HashMap}
import java.util.{MissingResourceException, ResourceBundle, Locale}

/*!# Configuration API

The `Circumflex` singleton can be used to set and retrieve
Circumflex configuration parameters in your application.

Configuration parameters are read from `cx.properties`
which should be available in classpath. The location of `cx.properties`
can be overridden with the `cx.configuration` System property.

This singleton is based on `KeyValueCoercion`
(see [[/core/src/main/scala/kvc.scala]]).

## Runtime configuration

You can also configure your application in runtime, just add your
configuration parameters into `Circumflex` just like you do it
with regular mutable Scala `Map`.

Note, however, that `Circumflex` singleton is not thread-safe,
so it is a best practice to only set configuration parameters
inside some initialization block which will be executed only
once by single thread.

Circumflex Maven Plugin enables you to configure your application
at build time using Maven properties (specified either in
application `pom.xml` or in `settings.xml`) and system properties
(specified in command line). This is very robust production scenario,
because it allows different configurations in different environments
without having to fall back to manual sources and resources filtering.
*/
object Circumflex extends HashMap[String, Any] with KeyValueCoercion {

  def locateBundle: Option[ResourceBundle] = try {
    Some(ResourceBundle.getBundle("cx"))
  } catch {
    case e: MissingResourceException =>
      CX_LOG.error("cx.properties not found in classpath. " +
          "Starting with empty configuration.")
      None
  }

  // The configuration object is initialized by reading `cx.properties`.
  try {
    locateBundle.map { bundle =>
      val keys = bundle.getKeys
      while (keys.hasMoreElements) {
        val k = keys.nextElement
        this(k) = bundle.getString(k)
      }
    }
  } catch {
    case e: Exception =>
      CX_LOG.error("Could not read configuration parameters from cx.properties.", e)
  }

  override def stringPrefix = "cx"
}