package ru.circumflex.orm

class Field[T](val record: Record,
               val column: Column[T]) {

  def get: Option[T] = record(column)

  def set(value: T): Unit = record.update(column,value)

  def setNull: Unit = record.update(column, None)

  override def toString = get match {
    case Some(value) => value.toString
    case None => ""
  }
}

