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


import java.sql.ResultSet

/**
 * Result set projection.
 */
trait Projection[T] {
  /**
   * Extract a value from result set.
   */
  def read(rs: ResultSet): Option[T]

  /**
   * SQL representation of this projection for use in SELECT clause.
   */
  def toSql: String

  override def toString = toSql
}

/**
 * Defines a contract for projections with assigned aliases
 * so that they may be used in ORDER BY/HAVING clauses.
 */
trait AliasedProjection[T] extends Projection[T] {
  def alias: String
}

class FieldProjection[T, R](val alias: String,
                            val node: RelationNode[R],
                            val column: Column[T, R])
    extends AliasedProjection[T] {

  def this(node: RelationNode[R], column: Column[T, R]) =
    this (node.alias + "_" + column.columnName, node, column)

  def read(rs: ResultSet): Option[T] =
    node.typeConverter.read(rs, alias).asInstanceOf[Option[T]]

  /**
   * Change an alias of this projection.
   */
  def as(alias: String) = new FieldProjection(alias, node, column)

  /**
   * Returns a column name qualified with node's alias.
   */
  def expr = node.dialect.qualifyColumn(column, node.alias)

  def toSql = node.dialect.columnAlias(column, alias, node.alias)

}

/**
 * Represents a projection for sequence's NEXTVAL clause.
 */
class SequenceNextValProjection[R](val seq: Sequence[R], val alias: String)
    extends AliasedProjection[Long] {

  def read(rs: ResultSet): Option[Long] =
    seq.typeConverter.read(rs, alias).asInstanceOf[Option[Long]]

  def toSql = seq.dialect.sequenceNextVal(seq, alias)
}

/**
 * Represents a projection for sequence's CURRVAL clause.
 */
class SequenceCurrValProjection[R](val seq: Sequence[R], val alias: String)
    extends AliasedProjection[Long] {

  def read(rs: ResultSet): Option[Long] =
    seq.typeConverter.read(rs, alias).asInstanceOf[Option[Long]]

  def toSql = seq.dialect.sequenceCurrVal(seq, alias)
}

class RecordProjection[R](val node: RelationNode[R])
    extends Projection[R] {
  val columnProjections: Seq[FieldProjection[Any, R]] =
  node.relation.columns.map(col => new FieldProjection(node, col.asInstanceOf[Column[Any, R]]))

  def read(rs: ResultSet): Option[R] = {
    val record = node.relation.recordClass
        .getConstructor()
        .newInstance()
        .asInstanceOf[Record[R]]
    columnProjections.foreach(
      p => record.setField(p.column, p.read(rs)))
    if (record.isIdentified) return Some(record.asInstanceOf[R])
    else return None
  }

  def toSql = node.dialect.selectClause(columnProjections.map(_.toSql): _*)
}