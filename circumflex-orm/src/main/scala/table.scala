/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

package ru.circumflex.orm

import collection.mutable.ListBuffer
import Query._

/**
 * Designates a relation that can be used to retrieve certain type of records.
 * It can be considered a table, a virtual table, a view, a subquery, etc.
 */
abstract class Relation[R] extends Configurable {

  /**
   * Contains a validation sequence that each record must pass on validation event.
   */
  var validators = new ListBuffer[RecordValidator[R]]

  /**
   * Returns a class of record which this relation describes.
   */
  def recordClass: Class[R]

  /**
   * The mandatory primary key constraint for this relation.
   */
  def primaryKey: PrimaryKey[_, R];

  /**
   * Unqualified relation name.
   */
  def relationName: String

  /**
   * Qualified relation name for use in SQL statements.
   */
  def qualifiedName: String

  /**
   * Returns columns that correspond to this relation.
   */
  def columns: Seq[Column[_, R]]

  /**
   * Returns all constraints defined for this relation.
   */
  def constraints: Seq[Constraint[R]]

  /**
   * Returns all associations defined for this relation.
   */
  def associations: Seq[Association[_, _]]

  /**
   * If possible, return an association from this relation as parent to
   * specified relation as child.
   */
  def getChildAssociation[C](child: Relation[C]): Option[Association[C, R]] =
    child.getParentAssociation(this)

  /**
   * If possible, return an association from this relation as child to
   * specified relation as parent.
   */
  def getParentAssociation[P](parent: Relation[P]): Option[Association[R, P]]

  /**
   * Returns column list excluding primary key column.
   */
  def nonPKColumns: Seq[Column[_, R]] =
    columns.filter(_ != primaryKey.column)

  /**
   * Returns a node that represents this relation.
   */
  def as(alias: String): RelationNode[R]

  /**
   * Creates a criteria object for this relation.
   */
  def createCriteria: Criteria[R] = new Criteria(this)

  def addFieldValidator(col: Column[_, R], validator: Validator): RecordValidator[R] = {
    val v = new RecordFieldValidator(col, validator)
    this.validators += v
    return v
  }

  /**
   * Returns None if record has passed validation. Otherwise returns
   * a <code>ValidationError</code> sequence.
   */
  def validate(record: Record[R]): Option[Seq[ValidationError]] = {
    val errors = validators.flatMap(_.apply(record))
    if (errors.size == 0) None
    else Some(errors)
  }

  /**
   * Throws <code>ValidationException</code> if record has failed validation.
   */
  def validate_!(record: Record[R]) = validate(record) match {
    case Some(errors) => throw new ValidationException(errors: _*)
    case _ =>
  }

  override def toString = qualifiedName

  override def equals(obj: Any) = obj match {
    case rel: Relation[R] =>
      rel.getClass.equals(this.getClass) &&
          rel.qualifiedName.equalsIgnoreCase(this.qualifiedName)
    case _ => false
  }

  override def hashCode = this.getClass.hashCode * 31 +
      this.qualifiedName.toLowerCase.hashCode
}

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
    with SchemaObject
    with JDBCHelper {
  private var _columns: Seq[Column[_, R]] = Nil
  private var _constraints: Seq[Constraint[R]] = Nil
  private var _cachedRecordClass: Class[R] = null;

  /**
   * Uses companion object runtime convention to find a record class.
   * Override it if you are not using companion objects.
   */
  def recordClass: Class[R] = {
    if (_cachedRecordClass == null)
      _cachedRecordClass = Class.forName(this.getClass.getName.replaceAll("(.*)\\$$", "$1"))
          .asInstanceOf[Class[R]]
    return _cachedRecordClass
  }

  /**
   * Returns all associations defined for this table.
   */
  def associations: Seq[Association[_, _]] = _constraints.flatMap {
    case a: Association[_, _] => Some(a)
    case _ => None
  }

  /**
   * Gets an association to parent by scanning declared foreign keys.
   */
  def getParentAssociation[P](relation: Relation[P]): Option[Association[R, P]] =
    associations.find(_.parentRelation == relation).asInstanceOf[Option[Association[R, P]]]

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

  def relationName = tableName

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
  def as(alias: String) = new TableNode(this, alias)

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

  /* SIMPLE QUERIES */

  /**
   * Queries a record by it's primary key.
   */
  def get(pk: Any): Option[R] =
    createCriteria.add(_.field(primaryKey.column) eq pk).unique

  /**
   * Queries all records.
   */
  def all(): Seq[R] =
    createCriteria.list

  /* HELPERS */

  /**
   * Helper method to create primary key constraint.
   */
  def pk[T](column: Column[T, R]): PrimaryKey[T, R] = {
    val pk = new PrimaryKey(this, column)
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
  def foreignKey[T, P](referenceTable: Table[P],
                       column: Column[T, R]): ForeignKey[T, R, P] = {
    val fk = new ForeignKey(this, referenceTable, column)
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

  /**
   * Helper method to create a boolean column.
   */
  def booleanColumn(name: String): BooleanColumn[R] = {
    val col = new BooleanColumn(this, name)
    addColumn(col)
    return col
  }

  /**
   * Helper method to create a timestamp column.
   */
  def timestampColumn(name: String): TimestampColumn[R] = {
    val col = new TimestampColumn(this, name)
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
abstract class GenericTable[R] extends Table[R] {
  val id = longColumn("id")
      .autoIncrement
      .notNull

  def primaryKey = pk(id)

}
