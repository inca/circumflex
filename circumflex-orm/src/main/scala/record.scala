package ru.circumflex.orm

abstract class Record {

  private var fieldsMap: Map[Column[_, _ <: Record], Any] = Map()

  def apply[T](col: Column[T, _]): Option[T] =
    fieldsMap.get(col.asInstanceOf[Column[_, _ <: Record]]).asInstanceOf[Option[T]]

  def update[T](col: Column[T, _], value: T): Unit =
    update(col, Some(value))

  def update[T](col: Column[T, _], value: Option[T]) = value match {
    case Some(value) => {
      fieldsMap += (col.asInstanceOf[Column[_, _ <: Record]] -> value)
    } case _ => {
      fieldsMap -= col.asInstanceOf[Column[_, _ <: Record]]
    }
  }

  def field[T](col: Column[T, _ <: Record]) = new Field(this, col)

  def relation: Relation[_ <: Record]

  def primaryKey: Option[_] = fieldsMap.get(relation.primaryKey.column)

  def isIdentified: Boolean = {
     primaryKey match {
      case None => return false
      case _ =>
    }
    return true
  }

}