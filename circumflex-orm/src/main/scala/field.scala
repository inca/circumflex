package ru.circumflex.orm


abstract class Field[T] {

  private var value: Option[T] = None

  def set(value: T): Unit = this.value = Some(value)

  def get: Option[T] = value

  override def toString = value match {
    case Some(v) => v.toString
    case _ => "null"
  }

}

class ColumnField[T](val column: Column[T, _])
    extends Field[T] {
}

class AssociationParentField[T](val association: Association[_, T])
    extends Field[T] {
}

