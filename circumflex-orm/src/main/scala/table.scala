package ru.circumflex.orm


import collection.mutable.Buffer

/**
 * Provides base functionality for generating domain model schema,
 * as well as validating, quering, inserting, deleting and updating
 * domain model objects (a.k.a. <code>records</code>).
 * In general there should be only one table instance per record class
 * (a singleton object, or, more conveniantly, the companion object).
 */
abstract class Table[R <: Record](val schemaName: String,
                                  val tableName: String) {

  //TODO make em private
  var columns: Seq[Column[_, R]] = Nil
  var constraints: Seq[Constraint[R]] = Nil

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

  /**
   * Returns a mandatory primary key constraint for this table.
   */
  def primaryKey: PrimaryKey[R];




  // HELPERS

  /**
   * Helper method to create primary key constraint.
   */
  def pk(columns: Column[_, R]*) =
    new PrimaryKey(this, columns.toList)

  /**
   * Helper method to create a bigint column.
   */
  def bigintColumn(name: String): BigintColumn[R] = {
    val col = new BigintColumn(this, name)
    columns ++= List(col)
    return col
  }

  /**
   * Helper method to create a string column.
   */
  def stringColumn(name: String): StringColumn[R] = {
    val col = new StringColumn(this, name)
    columns ++= List(col)
    return col
  }

}




