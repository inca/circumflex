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

/**
 * Represents an SQL view.
 */
abstract class View[R] extends Relation[R]
        with SchemaObject {

  /**
   * Views are not updatable by default.
   */
  override def readOnly = true

  /**
   * Returns view's query.
   */
  def query: SQLQuery

  protected[orm] def column[T](columnName: String): ViewColumn[T, R] = {
    val col = new ViewColumn[T, R](this, columnName)
    addColumns(col)
    return col
  }

  protected[orm] def inlineRecord[I](node: RelationNode[I]): ViewInlineRecord[I, R] = {
    val ir = new ViewInlineRecord(this, node)
    addColumns(ir.localColumns: _*)
    return ir
  }

  def as(alias: String) = new ViewNode(this, alias)

  def sqlDrop = dialect.dropView(this)

  def sqlCreate = dialect.createView(this)

  def objectName = qualifiedName
}

class ViewColumn[T, R](view: View[R], name: String)
        extends Column[T, R] (view, name, "")

class ViewInlineRecord[I, R](val view: View[R],
                             val node: RelationNode[I]) {
  private var _columnsMap: Map[Column[_, R], Column[_, I]] = Map()
  private var _localColumns: Seq[Column[_, R]] = Nil
  private var _pkColumn: Column[_, R] = null

  for (col <- node.columns) {
    val localCol = col.cloneForView(view)
    _localColumns ++= List(localCol)
    _columnsMap += (localCol.asInstanceOf[Column[_, R]] -> col)
    if (col == node.primaryKey.column)
      _pkColumn = localCol
  }

  if (_pkColumn == null)
    throw new ORMException("Could not inline record: no primary key column found.")

  def columnsMap = _columnsMap
  def localColumns = _localColumns
  def pkColumn = _pkColumn

}

class InlineRecordProxy[I, R](val record: Record[R],
                              val inlineRecord: ViewInlineRecord[I, R])
        extends Field[I] {

  def get: Option[I] = record.getField(inlineRecord.pkColumn) match {
    case Some(pk) =>             // instantiate only identified inlined records
      val r = inlineRecord.node.relation
              .recordClass
              .getConstructor()
              .newInstance()
              .asInstanceOf[Record[I]]
      inlineRecord.localColumns.foreach(col =>
        r.setField(
          inlineRecord.columnsMap(col).asInstanceOf[Column[Any,I]],
          record.getField(col)))
      Some(r.asInstanceOf[I])
    case _ => None
  }

  def set(value: I): Unit = {
    val r = value.asInstanceOf[Record[I]]
    inlineRecord.localColumns.foreach(col =>
      record.setField(
        col.asInstanceOf[Column[Any,R]],
        r.getField(inlineRecord.columnsMap(col))))
  }

  def setNull: Unit = {
    inlineRecord.localColumns.foreach(col => record.setField(col, None))
  }

}

