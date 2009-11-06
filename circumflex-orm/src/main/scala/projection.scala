package ru.circumflex.orm

/**
 * Result set projection.
 */
abstract class Projection[T](val alias: String)
    extends Configurable {

  override def toString = alias

}

class ColumnProjection[T](alias: String,
                          val column: Column[T, _])
    extends Projection[T](alias) {

}