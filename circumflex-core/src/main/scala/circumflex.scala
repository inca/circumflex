package ru.circumflex.core

import java.util.{ResourceBundle, Locale}
import collection.mutable.HashMap

/*!# Circumflex Configuration

This singleton can be used to retrieve Circumflex configuration parameters
throughout your application.

They are generally read from `cx.properties` available in classpath.

Note, that [Circumflex Maven Plugin](http://circumflex.ru/maven-plugin.html)
also enables you to configure your application on build using Maven properties
(specified either in application's `pom.xml` or in `settings.xml`) and system
properties (specified in command line). This is very robust production scenario,
because it allows different configurations in different environments without
manual filtering sources and resources.
*/
object Circumflex extends HashMap[String, Any] {

  try {
    val bundle = ResourceBundle.getBundle("cx", Locale.getDefault)
    val keys = bundle.getKeys
    while (keys.hasMoreElements) {
      val k = keys.nextElement
      this(k) = bundle.getString(k)
    }
  } catch {
    case _ => cxLog.error("Could not read configuration parameters from cx.properties.")
  }

  /*!
  This method is convenient for configuring different component implementations,
  such as configuring dialects or connection providers for
  [Circumflex ORM](http://circumflex.ru/orm.html).

  The logic is pretty simple: you provide either implementing object
  (if you run initialization code), or it's class (the object will be instantiated),
  or it's class name (class will be resolved using `Class.forName` and then the
  object will be instantiated). If exception occurs or none of above matched,
  the provided `default` is used.
  */

  def getObject[C](name: String, default: =>C): C = {
    def failback(className: String, e: Throwable): C = {
      cxLog.error("Configuration parameter '" + name + "'" +
          " resolved into " + className + "," +
          " but object instantiation failed. Defaults will be used.", e)
      default
    }
    this.get(name) match {
      case Some(obj: C) => obj
      case Some(c: Class[C]) => try {
        c.newInstance
      } catch {
        case e => failback(c.getName, e)
      }
      case Some(s: String) => try {
        Class.forName(s).newInstance.asInstanceOf[C]
      } catch {
        case e: Exception => failback(s, e)
      }
      case _ => default
    }
  }

}