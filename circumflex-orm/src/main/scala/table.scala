package ru.circumflex.orm

import reflect.Manifest

/**
 * Designates a relation that can be used to recover certain type of records.
 * It can be considered a table, a virtual table, a view, a subquery or everything
 * that may participate in FROM clause.
 */
abstract class Relation[R <: Record](implicit recordType: Manifest[R]) {
  /**
   * A record class recovered from type parameter.
   */
  def recordClass: Class[R] = Class.forName(recordType.toString).asInstanceOf[Class[R]]

  /**
   * The mandatory primary key constraint for this relation.
   */
  def primaryKey: PrimaryKey[R];

  /**
   * Qualified relation name for use in SQL statements.
   */
  def qualifiedName: String

  /**
   * If possible, return an association from this relation as parent to
   * specified relation as child.
   */
  def getChildAssociation[C <: Record](child: Relation[C]): Option[Association[C, R]]
  = child.getParentAssociation(this)

  /**
   * If possible, return an association from this relation as child to
   * specified relation as parent.
   */
  def getParentAssociation[P <: Record](parent: Relation[P]): Option[Association[R, P]]

  override def toString = qualifiedName
}

/**
 * Provides base functionality for generating domain model schema,
 * as well as validating, quering, inserting, deleting and updating
 * domain model objects (a.k.a. <code>records</code>).
 * In general there should be only one table instance per record class
 * (a singleton object, or, more conveniantly, the companion object).
 */
abstract class Table[R <: Record](implicit recordType: Manifest[R])
    extends Relation[R] with SchemaObject {

  private var _columns: Seq[Column[_, R]] = Nil
  private var _constraints: Seq[Constraint[R]] = Nil

  /**
   * Gets an association to parent by scanning declared foreign keys.
   */
  def getParentAssociation[P <: Record](relation: Relation[P]): Option[Association[R, P]] = {
    _constraints.foreach({
      case c: ForeignKey[R, P] =>
        if (c.referenceTable == relation)
          return Some(c)
      case _ =>
    })
    return None
  }

  /**
   * Returns Schema object, that will containt specified table.
   * Defaults to DefaultSchema singleton.
   */
  def schema: Schema = DefaultSchema

  /**
   * Provides schema name.
   */
  def schemaName: String = schema.schemaName

  /**
   * Provides table name. Defaults to unqualified record class name.
   */
  def tableName: String = recordClass.getSimpleName.toLowerCase

  def qualifiedName = dialect.tableName(this)

  /**
   * Table's column list.
   */
  def columns = _columns

  /**
   * Table's constraint list.
   */
  def constraints = _constraints

  /**
   * Creates an alias to use this table in SQL FROM clause.
   */
  def as(alias: String): TableNode[R] = new TableNode(this, alias)

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
  def pk(columns: Column[_, R]*): PrimaryKey[R] = {
    val pk = new PrimaryKey(this, columns.toList)
    return pk;
  }

  /**
   * Helper method to create unique constraint.
   */
  def unique(columns: Column[_, R]*): UniqueKey[R] = {
    val constr = new UniqueKey(this, columns.toList)
    addConstraint(constr)
    return constr
  }

  /**
   * Helper method to create a foreign key constraint.
   */
  def foreignKey[T <: Record](referenceTable: Table[T], columns: Column[_, R]*): ForeignKey[R, T] = {
    val fk = new ForeignKey(this, referenceTable, columns.toList)
    addConstraint(fk)
    return fk
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
