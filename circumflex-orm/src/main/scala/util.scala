package ru.circumflex.orm

import org.slf4j.LoggerFactory
import ORM._

/**
 * Contains helper constructions that automatically close such JDBC objects as
 * ResultSets and PreparedStatements.
 */
trait JDBCHelper {
  protected val sqlLog = LoggerFactory.getLogger("ru.circumflex.orm")

  def autoClose[A <: {def close(): Unit}, B](obj: A)
                                            (actions: A => B)
                                            (errors: Throwable => B): B =
    try {
      return actions(obj)
    } catch {
      case e => return errors(e)
    } finally {
      obj.close
    }

  def auto[A <: {def close(): Unit}, B](obj: A)
                                       (actions: A => B): B =
    autoClose(obj)(actions)(throw _)

}

object JDBCHelper extends JDBCHelper


trait SQLFragment extends SQLable {
  /**
   * Returns the parameters associated with this fragment.
   */
  def parameters: Seq[Any]

  /**
   * Renders this query by replacing parameter placeholders with actual values.
   */
  def toInlineSql: String = parameters.foldLeft(toSql)((sql, p) =>
    sql.replaceFirst("\\?", typeConverter.toString(p)))

}