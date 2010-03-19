package ru.circumflex.orm

trait SQLable {

  def toSql: String

  override def toString = toSql

}