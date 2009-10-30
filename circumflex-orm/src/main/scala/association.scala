package ru.circumflex.orm

/**
 * Provides functionality to handle one-to-many and many-to-one associations.
 */
class Association[C <: Record, P <: Record](val foreignKey: ForeignKey[C, P]) {

  def onDeleteNoAction: Association[C, P] = {
    foreignKey.onDelete = NoAction
    return this
  }

  def onDeleteCascade: Association[C, P] = {
    foreignKey.onDelete = CascadeAction
    return this
  }

  def onDeleteRestrict: Association[C, P] = {
    foreignKey.onDelete = RestrictAction
    return this
  }

  def onDeleteSetNull: Association[C, P] = {
    foreignKey.onDelete = SetNullAction
    return this
  }

  def onDeleteSetDefault: Association[C, P] = {
    foreignKey.onDelete = SetDefaultAction
    return this
  }

  def onUpdateNoAction: Association[C, P] = {
    foreignKey.onUpdate = NoAction
    return this
  }

  def onUpdateCascade: Association[C, P] = {
    foreignKey.onUpdate = CascadeAction
    return this
  }

  def onUpdateRestrict: Association[C, P] = {
    foreignKey.onUpdate = RestrictAction
    return this
  }

  def onUpdateSetNull: Association[C, P] = {
    foreignKey.onUpdate = SetNullAction
    return this
  }

  def onUpdateSetDefault: Association[C, P] = {
    foreignKey.onUpdate = SetDefaultAction
    return this
  }
}