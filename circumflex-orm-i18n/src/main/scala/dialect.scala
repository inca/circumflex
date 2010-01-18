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

import ru.circumflex.orm.{Relation, ForeignKey, Dialect}

class I18NDialect extends Dialect {

  def getLangExpression = "current_setting('" + ORMI18N.langSetting + "')"

  def setLangQuery(lang: String) =
    "set " + ORMI18N.langSetting + " = '" + lang.replaceAll("'","''") + "'"

  protected override def unwrap(relation: Relation[_]) = relation match {
      case lv: LocalizableView[_] => lv.rawTable
      case r: Relation[_] => r
    }

  def createInsertRule(rule: LocalizableViewInsertRule[_]) =
    "create or replace rule " + rule.objectName + " as " +
            "on insert to " + rule.localizableView.qualifiedName + " do instead (\n" +
            "insert into " + rule.localizableView.rawTable.qualifiedName + " (" +
            rule.localizableView
                    .columns
                    .map(_.columnName)
                    .mkString(", ") +
            ") values (" +
            rule.localizableView
                    .columns
                    .map(c => "new." + c.columnName)
                    .mkString(", ") +
            ");\n" + "insert into " + rule.localizableView.localeTable.qualifiedName +
            " (id, cx_lang, cx_item_id, " +
            rule.localizableView
                    .localizableColumns
                    .map(_.columnName)
                    .mkString(", ") +
            ") values (" + rule.localizableView.localeTable.idSeq.nextValSql + ", " +
            getLangExpression + ", new.id, " +
            rule.localizableView
                    .localizableColumns
                    .map(c => "new." + c.columnName)
                    .mkString(", ") + ");\n)"

  def dropInsertRule(rule: LocalizableViewInsertRule[_]) =
    "drop rule " + rule.objectName + "on " + rule.localizableView.qualifiedName

  def createUpdateRule(rule: LocalizableViewUpdateRule[_]) =
    "create or replace rule " + rule.objectName + " as " +
            "on update to " + rule.localizableView.qualifiedName +
            " do instead (\n" +
            "update " + rule.localizableView.rawTable.qualifiedName + " set " +
            rule.localizableView
                    .columns
                    .map(c => c.columnName + " = new." + c.columnName)
                    .mkString(", ") + " where id = old.id;\n" +
            "delete from " + rule.localizableView.localeTable.qualifiedName + " " +
            "where cx_lang = " + getLangExpression +
            " and cx_item_id = old.id;\n" +
            "insert into " + rule.localizableView.localeTable.qualifiedName +
            " (id, cx_lang, cx_item_id, " +
            rule.localizableView
                    .localizableColumns
                    .map(_.columnName)
                    .mkString(", ") +
            ") values (" + rule.localizableView.localeTable.idSeq.nextValSql + ", " +
            getLangExpression + ", new.id, " +
            rule.localizableView
                    .localizableColumns
                    .map(c => "new." + c.columnName)
                    .mkString(", ") +
            ");\n)"

  def dropUpdateRule(rule: LocalizableViewUpdateRule[_]) =
    "drop rule " + rule.objectName + " on " + rule.localizableView.qualifiedName

  def createDeleteRule(rule: LocalizableViewDeleteRule[_]) =
    "create or replace rule " + rule.objectName + " as " +
            "on delete to " + rule.localizableView.qualifiedName +
            " do instead " +
            "delete from " + rule.localizableView.rawTable.qualifiedName + " " +
            "where id = old.id"

  def dropDeleteRule(rule: LocalizableViewDeleteRule[_]) =
    "drop rule " + rule.objectName + " on " + rule.localizableView.qualifiedName

}
