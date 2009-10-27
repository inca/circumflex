package ru.circumflex.orm

class Field[T](val column: Column[T]) {

  var value: Option[T] = None
  
}