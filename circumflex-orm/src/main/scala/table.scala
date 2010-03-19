package ru.circumflex.orm

import ORM._


/**
 * Contains metadata necessary for generating domain model schema,
 * as well as quering, inserting, updating, validating and deleting
 * records.
 * In general there should be only one table instance per record class
 * (the best practice is to implement it with a companion object for
 * record class). However, it is also possible to create tables dynamically,
 * the only requirement is to implement the <code>recordClass</code> method.
 */
abstract class Table[R] extends Relation[R]
        with JDBCHelper
        with SchemaObject {

  def as(alias: String) = new TableNode(this).as(alias)

  /* DDL */

  /**
   * Produces SQL CREATE TABLE statement for this table.
   * Constraints are not included there.
   */
  def sqlCreate = dialect.createTable(this)

  /**
   * Produces SQL DROP TABLE statement.
   */
  def sqlDrop = dialect.dropTable(this)

  def objectName = qualifiedName
}

/**
 * Defines long primary key column "id" with sequence.
 */
trait LongIdPK[R] extends Relation[R] {
  val id = longColumn("id")
          .autoIncrement
          .notNull
  primaryKey(id)
}
