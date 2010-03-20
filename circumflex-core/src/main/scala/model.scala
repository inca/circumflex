package ru.circumflex.core

/**
 * A very simple model for operating with structural data.
 */
trait HashModel {
  def get(key: String): Option[Any]
}

/**
 * A very simple model for operating with wrappers.
 */
trait WrapperModel {
  def item: Any
}