package ru.circumflex.core

/**
 * Defines a very simple model for operating with structural data.
 */
trait HashModel {
  def get(key: String): Option[Any]
}