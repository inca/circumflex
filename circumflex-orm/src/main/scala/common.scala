package ru.circumflex.orm

import org.slf4j.LoggerFactory
import ORM._
import ru.circumflex.core.WrapperModel

// ## Common interfaces

/**
 * Simple interface for objects capable to render themselves into SQL statements.
 */
trait SQLable {
  def toSql: String
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

  override def toString = toSql
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

/**
 * *Value holder* is designed to be an extensible atomic data carrier unit
 * of record. It is subclassed by 'Field' and 'Association'.
 */
abstract class ValueHolder[T](val name: String, val uuid: String) extends WrapperModel {

  // An internally stored value.
  protected var _value: T = _

  // This way the value will be unwrapped by FTL engine.
  def item = getValue

  // Accessors and mutators.

  def getValue(): T = _value
  def apply(): T = getValue

  def setValue(newValue: T): this.type = {
    _value = newValue
    return this
  }
  def :=(newValue: T): this.type = setValue(newValue)
  def update(newValue: T): this.type = setValue(newValue)

  // Equality methods.

  override def equals(that: Any) = that match {
    case vh: ValueHolder[T] => vh.uuid == this.uuid
    case _ => false
  }

  override def hashCode = this.uuid.hashCode

  /**
   * Return a `String` representation of internal value.
   */
  def toString(default: String = "") = if (getValue == null) default else getValue.toString

  /**
   * Return `uuid` as this holder's identifier.
   */
  override def toString = uuid
}

/**
 * An action for `ON UPDATE` and `ON DELETE` clauses of
 * foreign key definitions.
 */
case class ForeignKeyAction(val toSql: String) extends SQLable {
  override def toString = toSql
}

/**
 * Join types for use in `FROM` clauses of SQL queries.
 */
case class JoinType(val toSql: String) extends SQLable {
  override def toString = toSql
}

/**
 * Set operations for use in SQL queries.
 */
case class SetOperation(val toSql: String) extends SQLable {
  override def toString = toSql
}

/**
 * An expression to use in `ORDER BY` clause.
 */
class Order(val expression: String, val parameters: Seq[Any])
    extends ParameterizedExpression {

  // Specificator (`ASC` or `DESC`).

  protected[orm] var _specificator = dialect.asc

  def asc: this.type = {
    this._specificator = dialect.asc
    return this
  }
  def ASC: this.type = asc

  def desc: this.type = {
    this._specificator = dialect.desc
    return this
  }
  def DESC: this.type = desc

  // Miscellaneous.

  def toSql = expression + " " + _specificator
}

// ## JDBC utilities

/**
 * Helper constructions that automatically close such JDBC objects as
 * `ResultSet`s and `PreparedStatement`s.
 */
object JDBC {
  protected[orm] val sqlLog = LoggerFactory.getLogger("ru.circumflex.orm")

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
