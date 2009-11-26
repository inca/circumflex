package ru.circumflex.core

import java.util.{MissingResourceException, ResourceBundle}
import org.slf4j.LoggerFactory

class Messages(val msgBundle: ResourceBundle) extends HashModel {

  val log = LoggerFactory.getLogger("ru.circumflex.core.messages")

  def get(key: String) = try {
    Some(msgBundle.getString(key))
  } catch {
    case e: MissingResourceException =>
      log.warn("Missing message for key {}, locale {}.", key, msgBundle.getLocale)
      Some("")
    case e => throw e
  }

}