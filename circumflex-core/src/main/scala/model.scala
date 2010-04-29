package ru.circumflex.core

/**
 * A very simple model for operating with structural data.
 */
trait HashModel {
  def apply(key: String): Option[Any]

  def apply[A](key: String, default: =>A): A = getOrElse(key, default)

  def get(key: String) = apply(key)
  
  def getOrElse[A](key: String, default: =>A): A = get(key) match {
    case Some(value: A) => value;
    case _ => default
  }
}

/**
 * A very simple model for operating with wrappers.
 */
trait WrapperModel {
  def item: Any
}