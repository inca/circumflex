package ru.circumflex.orm

import ORM._

/*!# Common interfaces

Circumflex ORM employs some basic constructs and interfaces, which are used
throughout the framework:

  * `SQLable` is an object capable of rendering themselves into SQL
  statements;
  * `ParameterizedExpression` is an expression with JDBC-style parameters;
  * `SchemaObject` is a database schema object capable of rendering itself into
  `sqlCreate` and `sqlDrop` DDL statements;

*/
trait SQLable {
  def toSql: String
}

trait ParameterizedExpression extends SQLable {

  /**
   * The parameters associated with this expression. The order is important.
   */
  def parameters: Seq[Any]

  /**
   * Renders this query by replacing parameter placeholders with actual values.
   */
  def toInlineSql: String = parameters.foldLeft(toSql)((sql, p) =>
    sql.replaceFirst("\\?", typeConverter.escape(p)))

  override def equals(that: Any) = that match {
    case e: ParameterizedExpression =>
      e.toSql == this.toSql && (e.parameters.toList -- this.parameters.toList) == Nil
    case _ => false
  }

  override def hashCode = 0

  override def toString = toSql
}

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
   * during schema creation by `DDL` to avoid duplicates and to print
   * nice messages on schema generation.
   *
   * We follow default convention to name objects:
   *
   *     <TYPE OF OBJECT> <qualified_name>
   *
   * where `TYPE OF OBJECT` is `TABLE`, `VIEW`, `SEQUENCE`, `TRIGGER`,
   * `FUNCTION`, `PROCEDURE`, `INDEX`, etc. (note the upper case), and
   * `qualified_name` is object's unique identifier.
   *
   * For equality testing, object names are taken in case-insensitive manner
   * (e.g. `MY_TABLE` and `my_table` are considered equal).
   */
  def objectName: String

  override def hashCode = objectName.toLowerCase.hashCode

  override def equals(obj: Any) = obj match {
    case so: SchemaObject => so.objectName.equalsIgnoreCase(this.objectName)
    case _ => false
  }

  override def toString = objectName
}
