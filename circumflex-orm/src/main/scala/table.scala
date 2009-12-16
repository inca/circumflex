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

import Query._
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
   * Table's constraints list.
   */
  def constraints: Seq[Constraint[R]] = _constraints

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
class GenericTable[R] extends Table[R] {

  val id = longColumn("id")
      .autoIncrement
      .notNull

  val idSeq = id.sequence.get

  def primaryKey = pk(id)

}
