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

import ORM._
import java.sql.ResultSet

/**
 * Result set projection.
 */
trait Projection[T] extends SQLable {
  /**
   * Extract a value from result set.
   */
  def read(rs: ResultSet): Option[T]

  /**
   * Return true for aggregate projections.
   * If at least one grouping projection presents in query, then all non-grouping projections
   * should appear to GROUP BY clause.
   */
  def grouping_?(): Boolean = false

  /**
   * Returns the list of aliases, from which this projection is composed.
   */
  def sqlAliases: Seq[String]

}

/**
 * Defines a contract for single-column projections with assigned aliases
 * so that they may be used in ORDER BY/HAVING clauses.
 */
trait AtomicProjection[T] extends Projection[T] {
  /**
   * Returns projection's alias
   */
  var alias: String = "this"

  def read(rs: ResultSet) = typeConverter.read(rs, alias).asInstanceOf[Option[T]]

  /**
   * Change an alias of this projection.
   */
  def as(alias: String): this.type = {
    this.alias = alias
    return this
  }

  def sqlAliases = List(alias)
}

/**
 * Defines a contract for multi-column projections that should be
 * read as a composite object.
 */
trait CompositeProjection[R] extends Projection[R] {

  def atomicProjections: Seq[AtomicProjection[_]]

  def sqlAliases = atomicProjections.flatMap(_.sqlAliases)

}

class ScalarProjection[T](val expression: String,
                          val grouping: Boolean)
        extends AtomicProjection[T] {

  override def grouping_?() = grouping

  def toSql = dialect.scalarAlias(expression, alias)
}

/**
 * Represents a projection for single field of a record.
 */
class ColumnProjection[T, R](val node: RelationNode[R],
                             val column: Column[T, R])
        extends AtomicProjection[T] {

  /**
   * Returns a column name qualified with node's alias.
   */
  def expr = dialect.qualifyColumn(column, node.alias)

  def toSql = dialect.columnAlias(column, alias, node.alias)
}

/**
 * Represents a projection for sequence's NEXTVAL clause.
 */
class SequenceNextValProjection[R](val seq: Sequence[R]) extends AtomicProjection[Long] {
  def toSql = dialect.sequenceNextVal(seq, alias)
}

/**
 * Represents a projection for sequence's CURRVAL clause.
 */
class SequenceCurrValProjection[R](val seq: Sequence[R]) extends AtomicProjection[Long] {
  def toSql = dialect.sequenceCurrVal(seq, alias)
}

/**
 * Represents a record projection (it groups all field projections).
 */
class RecordProjection[R](val node: RelationNode[R])
        extends CompositeProjection[R] {

  protected val _columnProjections: Seq[ColumnProjection[Any, R]] = node
          .relation
          .columns
          .map(col => new ColumnProjection(node, col.asInstanceOf[Column[Any, R]]))

  def atomicProjections = _columnProjections

  def read(rs: ResultSet): Option[R] = {
    val record = node.relation.recordClass
            .getConstructor()
            .newInstance()
            .asInstanceOf[Record[R]]
    _columnProjections.foreach(p => record.setField(p.column, p.read(rs)))
    if (record.isIdentified) return Some(record.asInstanceOf[R])
    else return None
  }

  def toSql = dialect.selectClause(_columnProjections.map(_.toSql): _*)
}
