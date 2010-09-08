package ru.circumflex.orm

/*!# Common interfaces

Circumflex ORM employs some basic constructs and interfaces, which are used
throughout the framework:

  * `SQLable` is an object capable of rendering themselves into SQL
  statements;
  * `ParameterizedExpression` is an expression with JDBC-style parameters;
  * `SchemaObject` is a database schema object capable of rendering itself into
  `sqlCreate` and `sqlDrop` DDL statements;
  * `ValueHolder` is an extensible atomic data carrier unit of record.

*/
trait SQLable {
  def toSql: String
}
