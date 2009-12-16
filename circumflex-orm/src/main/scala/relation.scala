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
import ORM._

/**
 * Designates a relation that can be used to retrieve certain type of records.
 * It can be considered a table, a virtual table, a view, a subquery, etc.
 */
trait Relation[R] {

  protected val _validators = new ListBuffer[RecordValidator[R]]
  protected val _columns = new ListBuffer[Column[_, R]]
  protected val _constraints = new ListBuffer[Constraint[R]]
  protected val _associations = new ListBuffer[Association[R, _]]

  private var _cachedRecordClass: Class[R] = null;

  /**
   * Returns a class of record which this relation describes.
   */
  def recordClass: Class[R] = {
    if (_cachedRecordClass == null)
      _cachedRecordClass = Class.forName(this.getClass.getName.replaceAll("(.*)\\$$", "$1"))
          .asInstanceOf[Class[R]]
    return _cachedRecordClass
  }

  /**
   * The mandatory primary key constraint for this relation.
   */
  def primaryKey: PrimaryKey[_, R];

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
   * Unqualified relation name. Defaults to unqualified record class name.
   */
  def relationName: String = recordClass.getSimpleName.toLowerCase

  /**
   * Returns relation's qualified name.
   */
  def qualifiedName: String = dialect.qualifyRelation(this)

  /**
   * Returns validators that correspond to this relation.
   */
  def validators: Seq[RecordValidator[R]] = _validators

  /**
   * Returns columns that correspond to this relation.
   */
  def columns: Seq[Column[_, R]] = _columns

  /**
   * Returns constraints that correspond to this relation.
   */
  def constraints: Seq[Constraint[R]] = _constraints

  /**
   * Returns associations that correspond to this relation.
   */
  def associations: Seq[Association[R, _]] = _associations

  /**
   * Returns sequences associated with this table.
   */
  def sequences = columns.flatMap(_.sequence)

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
  def getParentAssociation[P](relation: Relation[P]): Option[Association[R, P]] =
    associations.find(_.parentRelation == relation).asInstanceOf[Option[Association[R, P]]]

  /**
   * Returns column list excluding primary key column.
   */
  def nonPKColumns: Seq[Column[_, R]] =
    columns.filter(_ != primaryKey.column)

  /**
   * Returns a node that represents this relation.
   */
  def as(alias: String): RelationNode[R]

  /* SIMPLE QUERIES */

  /**
   * Creates a criteria object for this relation.
   */
  def createCriteria: Criteria[R] = new Criteria(this)

  /**
   * Queries a record by it's primary key.
   */
  def get(pk: Any): Option[R] =
    createCriteria.add(_.projection(primaryKey.column) eq pk).unique

  /**
   * Queries all records.
   */
  def all(): Seq[R] =
    createCriteria.list

  /**
   * Queries specified amount of records.
   */
  def all(limit: Int): Seq[R] =
    createCriteria.limit(limit).list

  /**
   * Queries specified amount of records, starting from specified offset.
   */
  def all(limit: Int, offset: Int): Seq[R] =
    createCriteria.limit(limit).offset(offset).list

  /* OBJECT DEFINITIONS */

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
    _constraints += constr
    return constr
  }

  /**
   * Helper method to create a foreign key constraint.
   */
  def foreignKey[T, P](parentRelation: Relation[P],
                       column: Column[T, R]): ForeignKey[T, R, P] = {
    val fk = new ForeignKey(this, parentRelation, column)
    _constraints += fk
    _associations += fk
    return fk
  }

  /**
   * Helper method to create a bigint column.
   */
  def longColumn(name: String): LongColumn[R] = {
    val col = new LongColumn(this, name)
    _columns += col
    return col
  }

  /**
   * Helper method to create a string column.
   */
  def stringColumn(name: String): StringColumn[R] = {
    val col = new StringColumn(this, name)
    _columns += col
    return col
  }

  /**
   * Helper method to create a boolean column.
   */
  def booleanColumn(name: String): BooleanColumn[R] = {
    val col = new BooleanColumn(this, name)
    _columns += col
    return col
  }

  /**
   * Helper method to create a timestamp column.
   */
  def timestampColumn(name: String): TimestampColumn[R] = {
    val col = new TimestampColumn(this, name)
    _columns += col
    return col
  }

  /* VALIDATION */

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

  def addFieldValidator(col: Column[_, R], validator: Validator): RecordValidator[R] = {
    val v = new RecordFieldValidator(col, validator)
    _validators += v
    return v
  }

  override def toString = qualifiedName

  override def equals(obj: Any) = obj match {
    case rel: Relation[R] => rel.qualifiedName.equalsIgnoreCase(this.qualifiedName)
    case _ => false
  }

  override def hashCode = this.qualifiedName.toLowerCase.hashCode
}