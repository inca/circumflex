package pro.savant.circumflex
package core

import collection.mutable.{ListBuffer, HashMap}
import collection.JavaConversions._
import java.util.{MissingResourceException, ResourceBundle}
import java.net.URL
import java.io.File
import java.util.jar.JarFile

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

  // The configuration object is initialized by reading `cx.properties`
  try {
    locateBundle.map { bundle =>
      val keys = bundle.getKeys
      while (keys.hasMoreElements) {
        val k = keys.nextElement
        this.update(k, bundle.getString(k))
      }
    }
  } catch {
    case e: Exception =>
      CX_LOG.error("Could not read configuration parameters from cx.properties.", e)
  }

  // Initial properties are overridden by System properties
  System.getProperties.stringPropertyNames.map { name =>
    this.update(name, System.getProperty(name))
  }

  override def stringPrefix = "cx"

  /*! ## ClassPath utils

  The `Circumflex` singleton also includes methods for working with
  class loaders:

    * `classLoader` returns the context class loader of current thread,
      which can be overridden using `cx.class.loader` context variable
      (see [[/core/src/main/scala/context.scala]]);

    * `searchClasses` scans specified `url` for classes with names matching
      specified `predicate` function (`url` can either be directory or JAR file,
      classes should be loadable by the class loader returned by the
      `classLoader` method).
  */

  def classLoader = ctx.getAs[ClassLoader]("cx.class.loader")
      .getOrElse(Thread.currentThread.getContextClassLoader)

  def searchClasses(url: URL,
                    predicate: String => Boolean): Seq[Class[_]] = {
    val file = new File(url.toURI)
    if (file.isDirectory)
      searchClassesDir(file, "", predicate)
    else if (file.getName.toLowerCase.endsWith(".jar"))
      searchClassesJar(file, predicate)
    else Nil
  }

  def searchClassesDir(file: File,
                       packagePrefix: String,
                       predicate: String => Boolean): Seq[Class[_]] =
    file.listFiles.flatMap { f =>
      val name = f.getName
      if (f.isDirectory)
        searchClassesDir(f, packagePrefix + name + ".", predicate)
      else if (isTopLevelClassFile(name)) {
        val className = packagePrefix + getClassName(name)
        if (predicate(className))
          safeLoadClass(className)
        else None
      } else None
    }

  def searchClassesJar(file: File,
                       predicate: String => Boolean): Seq[Class[_]] = {
    val jar = new JarFile(file)
    val entries = jar.entries
    val buffer = new ListBuffer[Class[_]]
    while (entries.hasMoreElements) {
      val entry = entries.nextElement()
      if (isTopLevelClassFile(entry.getName)) {
        val className = getClassName(entry.getName.replace('/', '.'))
        if (predicate(className))
          safeLoadClass(className).map(cl => buffer += cl)
      }
    }
    buffer.toSeq
  }

  protected def safeLoadClass(className: String): Option[Class[_]] =
    try {
      Some(classLoader.loadClass(className))
    } catch {
      case e: Exception =>
        CX_LOG.warn("Could not load class: " + className)
        None
    }

  def isTopLevelClassFile(filename: String) =
    filename.matches("[^\\$]+\\$?\\.class$")

  def getClassName(filename: String) =
    filename.substring(0, filename.length - 6)

}