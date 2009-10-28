package ru.circumflex.orm

/**
 * Provides base functionality for generating domain model schema,
 * as well as validating, quering, inserting, deleting and updating
 * domain model objects (a.k.a. <code>records</code>).
 * In general there should be only one table instance per record class
 * (a singleton object, or, more conveniantly, the companion object).
 */
abstract class Table(val schemaName: String,
                     val tableName: String) {

  //TODO how to organize columns, constraints and primary keys?

  /**
   * Configuration object is used for all persistence-related stuff.
   * Override it if you want to use your own configuration implementation.
   * @return DefaultConfiguration by default
   */
  def configuration: Configuration = DefaultConfiguration

  /**
   * Returns dialect-specific qualified name of a table.
   */
  def qualifiedName: String = configuration.dialect.tableQualifiedName(this)



}




