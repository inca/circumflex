package ru.circumflex.orm

/**
 * Defines a contract for parent-child (one-to-many) associations.
 * Usually they are modelled as tables with foreign keys.
 */
trait Association[C <: Record, P <: Record] {
  /**
   * Returns parent relation.
   */
  def parentRelation: Relation[P]

  /**
   * Returns child relation.
   */
  def childRelation: Relation[C]

  /**
   * Returns columns pairs to use in joining operations.
   * The first column in pair is from parent table, the second a matching one from child table.
   */
  def columnPairs: Seq[Pair[Column[_, P], Column[_, C]]]
}

/**
 * Provides functionality to handle one-to-many and many-to-one associations.
 */
class ForeignKeyAssociation[C <: Record, P <: Record](val foreignKey: ForeignKey[C, P])
    extends Association[C,P] {

  def parentRelation: Relation[P] = foreignKey.referenceTable
  def childRelation: Relation[C] = foreignKey.table

  private var _columnPairs: Seq[Pair[Column[_, P], Column[_, C]]] = Nil

  { // Initialize column pairs
    val localColSize = foreignKey.columns.size
    if (localColSize == 0)
      throw new IllegalArgumentException("Failed to initialize association " + foreignKey.constraintName +
          ": no local columns specified.")
    if (localColSize != foreignKey.referenceTable.primaryKey.columns.size)
      throw new IllegalArgumentException("Failed to initialize association " +
          foreignKey.constraintName + ": local columns count do not match referenced.")
    (0 until localColSize).foreach(i => {
      val pair: Pair[Column[_, P], Column[_, C]] = (foreignKey.columns(i),foreignKey.referenceTable.primaryKey.columns(i))
      _columnPairs ++= List(pair)
    })
  }

  def columnPairs = _columnPairs

  def onDeleteNoAction: ForeignKeyAssociation[C, P] = {
    foreignKey.onDelete = NoAction
    return this
  }

  def onDeleteCascade: ForeignKeyAssociation[C, P] = {
    foreignKey.onDelete = CascadeAction
    return this
  }

  def onDeleteRestrict: ForeignKeyAssociation[C, P] = {
    foreignKey.onDelete = RestrictAction
    return this
  }

  def onDeleteSetNull: ForeignKeyAssociation[C, P] = {
    foreignKey.onDelete = SetNullAction
    return this
  }

  def onDeleteSetDefault: ForeignKeyAssociation[C, P] = {
    foreignKey.onDelete = SetDefaultAction
    return this
  }

  def onUpdateNoAction: ForeignKeyAssociation[C, P] = {
    foreignKey.onUpdate = NoAction
    return this
  }

  def onUpdateCascade: ForeignKeyAssociation[C, P] = {
    foreignKey.onUpdate = CascadeAction
    return this
  }

  def onUpdateRestrict: ForeignKeyAssociation[C, P] = {
    foreignKey.onUpdate = RestrictAction
    return this
  }

  def onUpdateSetNull: ForeignKeyAssociation[C, P] = {
    foreignKey.onUpdate = SetNullAction
    return this
  }

  def onUpdateSetDefault: ForeignKeyAssociation[C, P] = {
    foreignKey.onUpdate = SetDefaultAction
    return this
  }
}