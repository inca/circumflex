package ru.circumflex.core

import java.util.{Locale, ResourceBundle}

class Messages(val baseName: String, val locale: Locale) extends HashModel {
  val msgBundle: ResourceBundle = try {
    ResourceBundle.getBundle(baseName, locale)
  } catch {
    case e => {
      cxLog.trace("ResourceBundle for messages instance not found: " + baseName)
      null
    }
  }
  def apply(key: String): Option[String] = try {
    msgBundle.getString(key)
  } catch {
    case e => None
  }
  def apply(key: String, params: Map[String, String]): Option[String] =
    apply(key) map {
      params.foldLeft(_) {
        case (m, (name, value)) => m.replaceAll("\\{" + name + "\\}", value)
      }
    }
  override def get(key: String): Option[String] = apply(key) match {
    case None =>
      cxLog.warn("Missing message for key {}, locale {}.", key, msgBundle.getLocale)
      ""
    case v => v
  }
  def get(key: String, params: Map[String, String]): Option[String] =
    apply(key, params) match {
      case None =>
        cxLog.warn("Missing message for key {}, locale {}.", key, msgBundle.getLocale)
        ""
      case v => v
    }
}

object Messages {
  def apply(): Messages = {
    if (!CircumflexContext.live_?)
      throw new CircumflexException("CircumflexContext is not available.")
    ctx('msg) match {
      case Some(m: Messages) => m
      case _ => throw new CircumflexException("Messages instance not found in CircumflexContext.")
    }
  }
}
