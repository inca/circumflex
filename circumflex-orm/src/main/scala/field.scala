package ru.circumflex.orm

class Field[T, R <: Record](val column: Column[T, R]) {

  var value: Option[T] = None
  
}