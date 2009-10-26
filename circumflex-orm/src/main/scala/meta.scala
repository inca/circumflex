package ru.circumflex.orm

class ORMException(msg: String) extends Exception(msg)

/**
 * Provides base functionality for all domain model objects.
 */
trait Record {

  /**
   * Configuration object used for all persistence-related stuff.
   * Override it if you want to provide your own configuration.
   * @return DefaultConfiguration by default
   */
  def configuration: Configuration = DefaultConfiguration

  def validate(): Unit = {}
  def save(): Unit = {}
  def saveOrUpdate(): Unit = {}
  def update(): Unit = {}
  def delete(): Unit = {}

}

trait MetaRecord extends Record {
  override def validate() = throw new IllegalAccessException("Method validate() cannot be called on metarecord.")
  def saveOrUpdate() = throw new IllegalAccessException("Method saveOrUpdate() cannot be called on metarecord.")
  def save() = throw new IllegalAccessException("Method save() cannot be called on metarecord.")
  def update() = throw new IllegalAccessException("Method update() cannot be called on metarecord.")
  def delete() = throw new IllegalAccessException("Method delete() cannot be called on metarecord.")
}


/**
 * Provides base functionality for persistent fields.
 */
abstract class Field[T](owner: Record) {

  private var value: Option[T] = None

  def apply(): T = this.value.get

  def update(value: T): Unit = {
    this.value = value
  }

}


class Person extends Record {

}

object Person extends Person with MetaRecord {

}