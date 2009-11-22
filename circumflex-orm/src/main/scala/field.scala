package ru.circumflex.orm

class Field[T](val record: Record,
               val column: Column[T]) {

  def get: Option[T] = record.getFieldValue(column)

  def set(value: T): Unit = record.setFieldValue(column,value)

  def setNull: Unit = record.setFieldValue(column, None)

  def <=(value: T): Unit = set(value)
  def :=(value: T): Unit = set(value)

  override def toString = get match {
    case Some(value) => value.toString
    case None => ""
  }
}

