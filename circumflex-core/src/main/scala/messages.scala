package ru.circumflex.core

import java.util.{Locale, ResourceBundle}

class Messages(val baseName: String, val locale: Locale) extends HashModel {
  val msgBundle: ResourceBundle = try {
    ResourceBundle.getBundle(baseName, locale)
  } catch {
    case e => {
      cxLog.debug("ResourceBundle for messages instance not found: " + baseName)
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
