package ru.circumflex.orm.i18n

import ru.circumflex.orm.{Relation, Dialect}

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
                    .map(c => if (c.default == None) "new." + c.columnName else "default")
                    .mkString(", ") +
            ");\n" + "insert into " + rule.localizableView.localeTable.qualifiedName +
            " (id, cx_lang, cx_item_id, " +
            rule.localizableView
                    .localizableColumns
                    .map(_.columnName)
                    .mkString(", ") +
            ") values (nextval('" + columnSequenceName(rule.localizableView.localeTable.id) + "'), " +
            getLangExpression + ", currval('" + columnSequenceName(rule.localizableView.id) + "'), " +
            rule.localizableView
                    .localizableColumns
                    .map(c => "new." + c.columnName)
                    .mkString(", ") + ");\n)"

  def dropInsertRule(rule: LocalizableViewInsertRule[_]) =
    "drop rule " + rule.objectName + " on " + rule.localizableView.qualifiedName

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
            ") values (nextval('" + columnSequenceName(rule.localizableView.localeTable.id) + "'), " +
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
