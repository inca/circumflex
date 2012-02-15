package pro.circumflex
package core

import collection.mutable.HashMap
import java.util.{MissingResourceException, ResourceBundle}

/*!# Circumflex Configuration

The `CircumflexCfg` singleton is used to retrieve Circumflex configuration parameters
throughout your application. It is essentially a `mutable.HashMap` with
`KeyValueCoercion`.

The shortcut `cx` for accessing Circumflex singleton can be used by importing
package object `core`:

``` {.scala}
import pro.circumflex._, core._
```

Configuration parameters are read from `cx.properties` which should be available
in classpath.

You can also configure your application in runtime, just add your configuration
parameters into `CircumflexCfg` using the methods of Scala's `Map` interface.
Note, however, that `CircumflexCfg` singleton is not thread-safe, so it is a best
practice to only set configuration parameters inside some initialization block
which will be executed only once by only one thread (e.g. `ServletContextListener`
in web application).

## Configuring with Maven properties

Circumflex Maven Plugin enables you to configure your application at build
time using Maven properties (specified either in application's `pom.xml` or
in `settings.xml`) and system properties (specified in command line).

This is achieved by executing the `cx:cfg` goal.

``` {.bash}
mvn cx:cfg
```

The goal essentially generates `cx.properties` into `target/classes`, populating
it with all properties known to Maven project.

This is a very robust production scenario, because it allows different
configurations in different environments and lets you avoid manual sources and
resources filtering.
*/

object CircumflexCfg extends HashMap[String, Any] with KeyValueCoercion {
  override def stringPrefix = "cx"

  def readFromBundle(name: String) {
    try {
      val bundle = ResourceBundle.getBundle(name)
      val keys = bundle.getKeys
      while (keys.hasMoreElements) {
        val k = keys.nextElement
        this.update(k, bundle.getString(k))
      }
    } catch {
      case e: MissingResourceException =>
        CX_LOG.warn("Could not find cx.properties in classpath. " +
            "Starting with empty configuration.")
    }
  }

  readFromBundle("cx")
}
