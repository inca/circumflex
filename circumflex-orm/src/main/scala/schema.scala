package ru.circumflex.orm

/**
 * Defines a contract for database schema objects.
 * They must provide SQL statement to create them and to drop them.
 */
trait SchemaObject {
  /**
   * Dialect object that is used to generate SQL statements.
   */
  def dialect: Dialect
  /**
   * SQL statement to create this database object.
   */
  def sqlCreate: String
  /**
   * SQL statement to drop this database object.
   */
  def sqlDrop: String
}

/**
 * Base functionality for SQL schema.
 */
class Schema(val schemaName: String) extends SchemaObject {
  /**
   * Configuration object is used for all persistence-related stuff.
   * Override it if you want to use your own configuration implementation.
   * @return DefaultConfiguration by default
   */
  def configuration: Configuration = DefaultConfiguration

  def dialect = configuration.dialect
  def sqlCreate = dialect.createSchema(this)
  def sqlDrop = dialect.dropSchema(this)
}

/**
 * Default public schema singleton; used to avoid another abstract method on Table.
 */
object DefaultSchema extends Schema("public")