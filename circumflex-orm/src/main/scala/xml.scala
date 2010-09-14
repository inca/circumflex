package ru.circumflex.orm

import xml.Elem

/*!# XML (de)serialization

Every data unit capable of (de)serializing itself (from)into XML should
extend the `XmlSerializable` trait.
 */
trait XmlSerializable[T] {
  def to(value: Option[T]): String
  def from(str: String): Option[T]
}