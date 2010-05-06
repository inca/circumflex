package ru.circumflex.orm

// ## XML (de-)serialization

trait XmlSerializable[T] {
  def to(value: T): String
  def from(string: String): T
}

