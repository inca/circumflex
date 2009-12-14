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
    this.validators += v
    return v
  }

  override def toString = qualifiedName

  override def equals(obj: Any) = obj match {
    case rel: Relation[R] =>
          rel.qualifiedName.equalsIgnoreCase(this.qualifiedName)
    case _ => false
  }

  override def hashCode = this.getClass.hashCode * 31 +
      this.qualifiedName.toLowerCase.hashCode
}