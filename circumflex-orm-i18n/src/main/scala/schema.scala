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
import collection.mutable.ListBuffer

/**
 * An updatable view for storing partially localizable data.
 */
abstract class LocalizableView[R] extends View[R] with LongIdPK[R] {

  /**
   * The view is updatable.
   */
  override def readOnly = false

  val rawTable: RawDataTable[R] = new RawDataTable(this)
  val localeTable: LocaleDataTable[R] = new LocaleDataTable(this)
  protected[orm] val localizableColumns = new ListBuffer[Column[_, R]]()

  /**
   * Add localizable columns.
   */
  def localize(cols: Column[_, R]*) = {
    localeTable.addColumns(cols: _*)
    localizableColumns ++= cols.toList
  }

  private val rNode = rawTable as "r"
  private val lNode = localeTable as "l"
  private val joinNode = rNode.join(lNode)
          .on("l.cx_lang = " + ORMI18N.getLangExpression)

  def projections = columns.map(col =>
    if (lNode.columns.contains(col))
      scalar("coalesce(l." + col.columnName + ", r." + col.columnName + ")")
    else scalar("r." + col.columnName))

  def query = select(projections: _*).from(joinNode)

  protected val ruleInsert = new LocalizableViewInsertRule(this)
  protected val ruleUpdate = new LocalizableViewUpdateRule(this)
  protected val ruleDelete = new LocalizableViewDeleteRule(this)

  addAuxiliaryObjects(rawTable, localeTable, ruleInsert, ruleUpdate, ruleDelete);

}

class LocaleDataTable[R](val localizableView: LocalizableView[R])
        extends Table[R] with LongIdPK[R] {

  override def schema = localizableView.schema
  override def relationName = localizableView.relationName + "_l"

  val lang = stringColumn("cx_lang")
          .notNull

  val item = longColumn("cx_item_id")
          .notNull
          .references(localizableView.rawTable)
          .onDeleteCascade
          .onUpdateCascade

  unique(lang, item.localColumn)

}

class RawDataTable[R](val localizableView: LocalizableView[R])
        extends Table[R] {

  override def recordClass = localizableView.recordClass
  override def schema = localizableView.schema
  override def relationName = localizableView.relationName + "_r"
  override def columns = localizableView.columns
  override def associations = localizableView.associations

  def primaryKey = localizableView.primaryKey

}

class LocalizableViewInsertRule[R](val localizableView: LocalizableView[R])
        extends SchemaObject {

  def objectName = localizableView.relationName + "_rule_ins"

  def sqlCreate = "create or replace rule " + objectName + " as\n\t" +
          "on insert to " + localizableView.qualifiedName + " do instead (\n\t\t" +
          "insert into " + localizableView.rawTable.qualifiedName + " (" +
          localizableView.columns
                  .map(_.columnName)
                  .mkString(", ") +
          ")\n\t\t\tvalues (" +
          localizableView.columns.map(c => "new." + c.columnName).mkString(", ") +
          ");\n\t\t" + "insert into " + localizableView.localeTable.qualifiedName +
          " (id, cx_lang, cx_item_id, " +
          localizableView.localizableColumns
                  .map(_.columnName)
                  .mkString(", ") +
          ")\n\t\t\tvalues (" + localizableView.localeTable.idSeq.nextValSql + ", " +
          ORMI18N.getLangExpression + ", new.id, " +
          localizableView.localizableColumns
                  .map(c => "new." + c.columnName)
                  .mkString(", ") + ");\n\t)"

  def sqlDrop = "drop rule " + objectName + "\n" +
          "on " + localizableView.qualifiedName
}

class LocalizableViewUpdateRule[R](val localizableView: LocalizableView[R])
        extends SchemaObject {

  def objectName = localizableView.relationName + "_rule_upd"

  def sqlCreate = "create or replace rule " + objectName + " as\n\t" +
          "on update to " + localizableView.qualifiedName +
          " do instead (\n\t\t" +
          "update " + localizableView.rawTable.qualifiedName + " set\n\t\t\t" +
          localizableView.columns
                  .map(c => c.columnName + " = new." + c.columnName)
                  .mkString(",\n\t\t\t") + "\n\t\t\twhere id = old.id;\n\t\t" +
          "delete from " + localizableView.localeTable.qualifiedName + "\n\t\t\t" +
          "where cx_lang = " + ORMI18N.getLangExpression +
          " and cx_item_id = old.id;\n\t\t" +
          "insert into " + localizableView.localeTable.qualifiedName +
          " (id, cx_lang, cx_item_id, " +
          localizableView.localizableColumns
                  .map(_.columnName)
                  .mkString(", ") +
          ")\n\t\t\tvalues (" + localizableView.localeTable.idSeq.nextValSql + ", " +
          ORMI18N.getLangExpression + ", new.id, " +
          localizableView.localizableColumns
                  .map(c => "new." + c.columnName)
                  .mkString(", ") +
          ");\n\t)"

  def sqlDrop = "drop rule " + objectName + "\n" +
          "on " + localizableView.qualifiedName
}

class LocalizableViewDeleteRule[R](val localizableView: LocalizableView[R])
        extends SchemaObject {

  def objectName = localizableView.relationName + "_rule_del"

  def sqlCreate = "create or replace rule " + objectName + " as\n\t" +
          "on delete to " + localizableView.qualifiedName +
          " do instead\n\t\t" +
          "delete from " + localizableView.rawTable.qualifiedName + "\n\t\t\t" +
          "where id = old.id"

  def sqlDrop = "drop rule " + objectName + "\n" +
          "on " + localizableView.qualifiedName
}

