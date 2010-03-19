package ru.circumflex.core

import java.util.{Locale, MissingResourceException, ResourceBundle}
import org.slf4j.LoggerFactory

class Messages(val baseName: String, val locale: Locale) extends HashModel {

  val log = LoggerFactory.getLogger("ru.circumflex.core")

  val msgBundle: ResourceBundle = try {
    ResourceBundle.getBundle(baseName, locale)
  } catch {
    case e => {
      log.trace("ResourceBundle for messages instance not found: " + baseName)
      null
    }
  }

  def apply(key: String): Option[String] = try {
    Some(msgBundle.getString(key))
  } catch {
    case e => None
  }

  def apply(key: String, params: collection.Map[String, String]): Option[String] =
    apply(key) match {
      case Some(msg) => Some(params.foldLeft(msg) {
        (m, p) => m.replaceAll("\\{" + p._1 + "\\}", p._2)
      })
      case None => None
    }

  def get(key: String) = apply(key) match {
    case None =>
      log.warn("Missing message for key {}, locale {}.", key, msgBundle.getLocale)
      Some("")
    case v => v
  }

  def get(key: String, params: collection.Map[String, String]) =
    apply(key, params) match {
      case None =>
        log.warn("Missing message for key {}, locale {}.", key, msgBundle.getLocale)
        Some("")
      case v => v
    }
}

object Messages {
  def apply(): Messages = {
    if (Circumflex.ctx == null)
      throw new CircumflexException("CircumflexContext is not available.")
    Circumflex.ctx.get("msg") match {
      case Some(m: Messages) => m
      case _ => throw new CircumflexException("Messages instance not found in CircumflexContext.")
    }
  }
}
