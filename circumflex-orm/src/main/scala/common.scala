package ru.circumflex.orm

/*!# Common interfaces

Circumflex ORM employs some basic constructs and interfaces, which are used
throughout the framework:

  * `SQLable` represents objects capable of rendering themselves into SQL
  statements;
  * `ParameterizedExpression` represents expression with JDBC-style parameters;
  * `SchemaObject` represents a database schema objects capable of rendering
  `sqlCreate` and `sqlDrop` DDL statements;
  * `ValueHolder` is an extensible atomic data carrier unit of record.

*/
trait SQLable {
  def toSql: String
}

