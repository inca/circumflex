package ru.circumflex.orm

abstract class Record {

  var fieldsMap: Map[Column[_, _ <: Record], Any] = Map()

  def field[T](column: Column[T, _ <: Record]) = new Field(this, column)

}