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

  private var _columns: Seq[Column[_, R]] = Nil
  private var _constraints: Seq[Constraint[R]] = Nil

  /**
   * Configuration object is used for all persistence-related stuff.
   * Override it if you want to use your own configuration implementation.
   * @return DefaultConfiguration by default
   */
  def configuration: Configuration = DefaultConfiguration

  /**
   * Proxy to fast-retrieve dialect.
   */
  def dialect: Dialect = configuration.dialect

  /**
   *  Dialect-specific qualified name of a table.
   */
  def qualifiedName: String = configuration.dialect.tableQualifiedName(this)

  /**
   * The mandatory primary key constraint for this table.
   */
  def primaryKey: PrimaryKey[R];

  /**
   * Table's column list.
   */
  def columns = _columns

  /**
   * Table's constraint list.
   */
  def constraints = _constraints

  /**
   * Adds some columns to this table.
   */
  def addColumn(cols: Column[_, R]*) =
    _columns ++= cols.toList

  /**
   * Adds some constraints to this table.
   */
  def addConstraint(constrs: Constraint[R]*) =
    _constraints ++= constrs.toList

  /* HELPERS */

  /**
   * Helper method to create primary key constraint.
   */
  def pk(columns: Column[_, R]*) =
    new PrimaryKey(this, columns.toList)

  /**
   * Helper method to create unique constraint (return table instance for chaining).
   */
  def unique(columns: Column[_, R]*): Table[R] = {
    val constr = new UniqueKey(this, columns.toList)
    addConstraint(constr)
    return this
  }

  /**
   * Helper method to create a bigint column.
   */
  def longColumn(name: String): LongColumn[R] = {
    val col = new LongColumn(this, name)
    addColumn(col)
    return col
  }

  /**
   * Helper method to create a string column.
   */
  def stringColumn(name: String): StringColumn[R] = {
    val col = new StringColumn(this, name)
    addColumn(col)
    return col
  }

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

  /**
   * Produces a list of SQL ALTER TABLE ADD CONSTRAINT statements.
   */
  def sqlCreateConstraints: Seq[String] =
    constraints.map(_.sqlCreate)

  /**
   * Produces a list of SQL ALTER TABLE DROP CONSTRAINT statements.
   */
  def sqlDropConstraints: Seq[String] =
    constraints.map(_.sqlDrop)

}




