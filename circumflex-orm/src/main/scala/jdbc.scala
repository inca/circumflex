package ru.circumflex.orm


import java.sql.Connection

/**
 * Contains helper constructions that automatically close such JDBC objects as
 * ResultSets and PreparedStatements.
 */
trait JDBCHelper {

  def using[A <: {def close(): Unit}, B](obj: A)
                                        (actions: A => B)
                                        (errors: Throwable => B): B =
    try {
      return actions(obj)
    } catch {
      case e => return errors(e)
    } finally {
      obj.close
    }

}