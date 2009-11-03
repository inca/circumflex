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
}

/**
 * Provides functionality to handle one-to-many and many-to-one associations.
 */
class ForeignKeyAssociation[C <: Record, P <: Record](val foreignKey: ForeignKey[C, P])
    extends Association[C,P] {

  def parentRelation: Relation[P] = foreignKey.referenceTable
  def childRelation: Relation[C] = foreignKey.table

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