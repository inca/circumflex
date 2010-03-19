package ru.circumflex.orm.i18n

import ru.circumflex.orm._
import ORM._
import ORMI18N._
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

  /**
   * Returns all constraints and auxiliary objects defined for this view.
   */
  override def postAuxiliaryObjects: Seq[SchemaObject] =
    _postAuxiliaryObjects ++ constraints

  private val rNode = rawTable as "r"
  private val lNode = localeTable as "l"
  private val joinNode = rNode.join(lNode)
          .on("r.id = l.cx_item_id and l.cx_lang = " + i18nDialect.getLangExpression)

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
  override def primaryKey = localizableView.primaryKey
}

class LocalizableViewInsertRule[R](val localizableView: LocalizableView[R])
        extends SchemaObject {
  def objectName = localizableView.relationName + "_rule_ins"
  def sqlCreate = i18nDialect.createInsertRule(this)
  def sqlDrop = i18nDialect.dropInsertRule(this)
}

class LocalizableViewUpdateRule[R](val localizableView: LocalizableView[R])
        extends SchemaObject {
  def objectName = localizableView.relationName + "_rule_upd"
  def sqlCreate = i18nDialect.createUpdateRule(this)
  def sqlDrop = i18nDialect.dropUpdateRule(this)
}

class LocalizableViewDeleteRule[R](val localizableView: LocalizableView[R])
        extends SchemaObject {
  def objectName = localizableView.relationName + "_rule_del"
  def sqlCreate = i18nDialect.createDeleteRule(this)
  def sqlDrop = i18nDialect.dropDeleteRule(this)
}

