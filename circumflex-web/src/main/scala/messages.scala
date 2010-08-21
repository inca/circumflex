package ru.circumflex.core

import java.util.{Locale, ResourceBundle}

/*!

# Messages API

Messages API present you a convenient way to internationalize your application.

Generally, all strings that should be presented to user are stored in
separate `.properties`-files as suggested by [Java Internationalization][java-i18n].
Such strings are resolved using `Messages` class or `msg` helper object.

Circumflex Messages API goes beyound this simple approach and offers [messages grouping](#grouping),
[ranged resolving](#range), [parameters interpolation](#params) and [formatting](#format).

    [java-i18n]: http://java.sun.com/javase/technologies/core/basic/intl

*/

class Message(val key: String, var params: Map[String, Object]) {

  def this(key: String, params: Pair[String, Object]*) =
    this(key, Map(params: _*))

}

class Messages(val bundleName: String, val locale: Locale) extends HashModel {
  val msgBundle: ResourceBundle = try {
    ResourceBundle.getBundle(bundleName, locale)
  } catch {
    case e => {
      cxLog.debug("ResourceBundle for messages instance not found: " + bundleName)
      null
    }
  }
  def get(key: String): Option[String] = try {
    msgBundle.getString(key)
  } catch {
    case e => None
  }
  def get(key: String, params: Pair[String, String]*): Option[String] = get(key, Map(params: _*))
  def get(key: String, params: Map[String, String]): Option[String] =
    get(key).map(m => params.foldLeft(m) {
      case (m, (name, value)) => m.replaceAll("\\{" + name + "\\}", value)
    })
  override def apply(key: String): String = get(key) match {
    case Some(v) => v
    case _ =>
      cxLog.warn("Missing message for key {}, locale {}.", key, msgBundle.getLocale)
      ""
  }
  def apply(key: String, params: Pair[String, String]*): String = apply(key, Map(params: _*))
  def apply(key: String, params: Map[String, String]): String = get(key, params) match {
    case Some(v) => v
    case _ =>
      cxLog.warn("Missing message for key {}, locale {}.", key, msgBundle.getLocale)
      ""
  }
}
