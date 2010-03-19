package ru.circumflex.core

import javax.servlet.http._
import scala.collection.mutable.HashMap
import Circumflex._

/**
 * A helper for setting flashes. Flashes provide a way to pass temporary objects between requests.
 */
class FlashHelper extends HashModel {

  private val _key = "cx.flash"

  def apply(key: String): Option[Any] = {
    val flashMap = ctx.request.getSession.getAttribute(_key) match {
      case m: HashMap[String, Any] => m
      case _ => new HashMap[String, Any]()
    }
    flashMap.get(key) match {
      case Some(value) =>
        ctx.request.getSession.setAttribute(_key, flashMap - key)
        return Some(value)
      case _ => return None
    }
      
  }

  def update(key: String, value: Any): Unit = {
    val flashMap = ctx.request.getSession.getAttribute(_key) match {
      case m: HashMap[String, Any] => m
      case _ => new HashMap[String, Any]()
    }
    ctx.request.getSession.setAttribute(_key, flashMap + (key -> value))
  }

  def get(key: String) = apply(key)

}
