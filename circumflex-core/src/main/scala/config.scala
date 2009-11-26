package ru.circumflex.core

import java.util.ResourceBundle

/**
 * Defines a contract for every object that can be configured.
 */
trait Configurable {
  /**
   * Returns a default configuration resource bundle.
   */
  def configurationBundle = ResourceBundle.getBundle("cx")

}