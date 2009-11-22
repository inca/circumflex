package ru.circumflex.orm

/**
 * Designates a relation that can be used to recover certain type of records.
 * It can be considered a table, a virtual table, a view, a subquery or everything
 * that may participate in FROM clause.
 */
abstract class Relation extends Configurable {

  /**
   * Returns a class of record which this relation describes.
   */
  def recordClass: Class[_ <: Record]

  /**
   * The mandatory primary key constraint for this relation.
   */
  def primaryKey: PrimaryKey;

  /**
   * Qualified relation name for use in SQL statements.
   */
  def qualifiedName: String

  /**
   * If possible, return an association from this relation as parent to
   * specified relation as child.
   */
  def getChildAssociation(child: Relation): Option[Association] =
    child.getParentAssociation(this)

  /**
   * Returns columns that correspond to this relation.
   */
  def columns: Seq[Column[_]]

  /**
   * Returns all associations defined for this relation.
   */
  def associations: Seq[Association]

  /**
   * If possible, return an association from this relation as child to
   * specified relation as parent.
   */
  def getParentAssociation(parent: Relation): Option[Association]

  /**
   * Returns column list excluding primary key column.
   */
  def nonPKColumns: Seq[Column[_]] =
      columns.filter(_ != primaryKey.column)

  override def toString = qualifiedName

  override def equals(obj: Any) = obj match {
    case rel: Relation =>
      rel.getClass.equals(this.getClass) &&
          rel.qualifiedName.equalsIgnoreCase(this.qualifiedName)
    case _ => false
  }

  override def hashCode = this.getClass.hashCode * 31 +
      this.qualifiedName.toLowerCase.hashCode
}

/**
 * Provides base functionality for generating domain model schema,
 * as well as validating, quering, inserting, deleting and updating
 * domain model objects (a.k.a. <code>records</code>).
 * In general there should be only one table instance per record class
 * (a singleton object, or, more conveniantly, the companion object).
 */
abstract class Table extends Relation
    with SchemaObject
    with JDBCHelper {

  private var _columns: Seq[Column[_]] = Nil
  private var _constraints: Seq[Constraint] = Nil

  /**
   * Returns all associations defined for this table.
   */
  def associations: Seq[Association] = _constraints.flatMap {
    case a: Association => Some(a)
    case _ => None
  }

  /**
   * Gets an association to parent by scanning declared foreign keys.
   */
  def getParentAssociation(relation: Relation): Option[Association] =
    associations.find(_.parentRelation == relation)

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
   * List of sequences associated with this table.
   */
  def sequences = columns.flatMap(_.sequence)

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
  def as(alias: String): TableNode = new TableNode(this, alias)

  /**
   * Adds some columns to this table.
   */
  def addColumn(cols: Column[_]*) =
    _columns ++= cols.toList

  /**
   * Adds some constraints to this table.
   */
  def addConstraint(constrs: Constraint*) =
    _constraints ++= constrs.toList

  /* HELPERS */

  /**
   * Helper method to create primary key constraint.
   */
  def pk(column: Column[_]): PrimaryKey = {
    val pk = new PrimaryKey(this, column)
    return pk;
  }

  /**
   * Helper method to create unique constraint.
   */
  def unique(columns: Column[_]*): UniqueKey = {
    val constr = new UniqueKey(this, columns.toList)
    addConstraint(constr)
    return constr
  }

  /**
   * Helper method to create a foreign key constraint.
   */
  def foreignKey(referenceTable: Table,
                 column: Column[_]): ForeignKey = {
    val fk = new ForeignKey(this, referenceTable, column)
    addConstraint(fk)
    return fk
  }

  /**
   * Helper method to create a bigint column.
   */
  def longColumn(name: String): LongColumn = {
    val col = new LongColumn(this, name)
    addColumn(col)
    return col
  }

  /**
   * Helper method to create a string column.
   */
  def stringColumn(name: String): StringColumn = {
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

}

/**
 * Just a helper that defines long primary key column "id" with sequence.
 */
abstract class GenericTable extends Table {

  val id = longColumn("id")
      .autoIncrement
      .notNull

  def primaryKey = pk(id)

}
