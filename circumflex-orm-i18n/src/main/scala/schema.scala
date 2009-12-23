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

package ru.circumflex.orm.i18n

import ru.circumflex.orm._
import Query._

/**
 * An updatable view for storing partially localizable data.
 */
abstract class LocalizableTable[R]
        extends Table[R]
                with LongIdPK[R] {

  val localeTable: LocaleDataTable[R] = new LocaleDataTable(this)
  val localizedView: LocalizedView[R] = new LocalizedView(this)

  /**
   * Add localizable columns.
   */
  def localize(cols: Column[_, R]*) =
    localeTable.addColumns(cols: _*)

  override def insert_!(record: Record[R]) =
    localizedView.insert_!(record)
  override def update_!(record: Record[R]) =
    localizedView.update_!(record)
  override def delete(record: Record[R]) =
    localizedView.delete(record)

}

class LocaleDataTable[R](val localizableTable: LocalizableTable[R])
        extends Table[R]
                with LongIdPK[R] {

  override def schema = localizableTable.schema
  override def relationName = localizableTable.relationName + "_l"

  val lang = stringColumn("cx_lang")
          .notNull

  val item = longColumn("cx_item_id")
          .notNull
          .references(localizableTable)
          .onDeleteCascade
          .onUpdateCascade

  unique(lang, item.localColumn)

}

class LocalizedView[R](val localizableTable: LocalizableTable[R])
        extends View[R] {

  override def readOnly = false
  override def recordClass = localizableTable.recordClass
  override def schema = localizableTable.schema
  override def relationName = localizableTable.relationName + "_localized"
  override def columns = localizableTable.columns
  def primaryKey = localizableTable.primaryKey

  private val tNode = localizableTable as "t"
  private val lNode = localizableTable.localeTable as "l"
  private val joinNode = tNode.join(lNode)
          .on("l.cx_lang = " + ORMI18N.getLangExpression)

  def projections = columns.map(col =>
    if (lNode.columns.contains(col))
      scalar("coalesce(l." + col.columnName + ", t." + col.columnName + ")")
    else scalar("t." + col.columnName))

  def query = select(projections: _*).from(joinNode)

}


