package ru.circumflex.orm

class Field[T](val record: Record,
               val column: Column[T, _ <: Record]) {

  def get: Option[T] =
    record.fieldsMap.get(column).asInstanceOf[Option[T]]

  def set(value: T): Unit =
    record.fieldsMap += (column.asInstanceOf[Column[_, _ <: Record]] -> value)

  def setNull: Unit = record.fieldsMap -= column

}

