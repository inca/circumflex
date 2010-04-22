package ru.circumflex.orm

import org.slf4j.LoggerFactory
import ORM._

// ## Common interfaces

/**
 * Simple interface for objects capable to render themselves into SQL statements.
 */
trait SQLable {
  def toSql: String
  override def toString = toSql
}

/**
 * Simple interface for expressions with JDBC-style parameters
 */
trait ParameterizedExpression extends SQLable {
  /**
   * The parameters associated with this expression. The order is important.
   */
  def parameters: Seq[Any]

  /**
   * Render this query by replacing parameter placeholders with actual values.
   */
  def toInlineSql: String = parameters.foldLeft(toSql)((sql, p) =>
    sql.replaceFirst("\\?", typeConverter.toString(p)))

}

/**
 * Simple interface for database objects capable to render themselves into DDL
 * CREATE and DROP statements.
 */
trait SchemaObject {
  /**
   * SQL statement to create this database object.
   */
  def sqlCreate: String

  /**
   * SQL statement to drop this database object.
   */
  def sqlDrop: String

  /**
   * SQL object name. It is used to uniquely identify this object
   * during schema creation by `DDLExport` to avoid duplicates.
   * Object names are case-insensitive (e.g. `MY_TABLE` and `my_table` are
   * considered equal).
   */
  def objectName: String

  override def hashCode = objectName.toLowerCase.hashCode

  override def equals(obj: Any) = obj match {
    case so: SchemaObject => so.objectName.equalsIgnoreCase(this.objectName)
    case _ => false
  }

  override def toString = objectName
}

// ## JDBC utilities

/**
 * Helper constructions that automatically close such JDBC objects as
 * `ResultSet`s and `PreparedStatement`s.
 */
object JDBC {
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

// ## Exceptions

/**
 * The most generic exception class. 
 */
class ORMException(msg: String, cause: Throwable) extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)
  def this(cause: Throwable) = this(null, cause)
}