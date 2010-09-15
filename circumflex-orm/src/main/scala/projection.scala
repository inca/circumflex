package ru.circumflex.orm

import java.sql.ResultSet

/*!# Projections

In relational algebra a *projection* is a function which describes a subset of
columns returned from an SQL query. In Circumflex ORM instances of the `Projection`
trait are used to process `ResultSet` and determine the result type of SQL queries.
*/
trait Projection[T] extends SQLable {

  /**
   * Extract a value from result set.
   */
  def read(rs: ResultSet): T

  /**
   * Returns the list of aliases, from which this projection is composed.
   */
  def sqlAliases: Seq[String]

  override def toString = toSql
}