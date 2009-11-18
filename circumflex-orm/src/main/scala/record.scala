package ru.circumflex.orm

/**
 * Represents records that could be recovered from relations.
 */
abstract class Record {

  private var fieldsMap: Map[Column[_], Any] = Map()

  def apply[T](col: Column[T]): Option[T] =
    fieldsMap.get(col).asInstanceOf[Option[T]]

  def update[T](col: Column[T], value: T): Unit =
    update(col, Some(value))

  def update[T](col: Column[T], value: Option[T]) = value match {
    case Some(value) => {
      fieldsMap += (col -> value)
    } case _ => {
      fieldsMap -= col
    }
  }

  def field[T](col: Column[T]) = new Field(this, col)

  def relation: Relation

  def primaryKey: Option[_] = fieldsMap.get(relation.primaryKey.column)

  def isIdentified = primaryKey match {
    case None => false
    case _ => true
  }

}