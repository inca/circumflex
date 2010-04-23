package ru.circumflex.orm

import ru.circumflex.core.WrapperModel

// ## Association

abstract class Assocation[T <: Record[T], R <: Record[R]](val record: T,
                                                          val referenceRelation: Relation[R])
    extends ValueHolder[T, R] {

  protected var _onDelete: ForeignKeyAction = NoAction
  protected var _onUpdate: ForeignKeyAction = NoAction

  def onDelete = _onDelete
  def onUpdate = _onUpdate

  def onDeleteNoAction: this.type = {
    _onDelete = NoAction
    return this
  }

  def onDeleteCascade: this.type = {
    _onDelete = CascadeAction
    return this
  }

  def onDeleteRestrict: this.type = {
    _onDelete = RestrictAction
    return this
  }

  def onDeleteSetNull: this.type = {
    _onDelete = SetNullAction
    return this
  }

  def onDeleteSetDefault: this.type = {
    _onDelete = SetDefaultAction
    return this
  }

  def onUpdateNoAction: this.type = {
    _onUpdate = NoAction
    return this
  }

  def onUpdateCascade: this.type = {
    _onUpdate = CascadeAction
    return this
  }

  def onUpdateRestrict: this.type = {
    _onUpdate = RestrictAction
    return this
  }

  def onUpdateSetNull: this.type = {
    _onUpdate = SetNullAction
    return this
  }

  def onUpdateSetDefault: this.type = {
    _onUpdate = SetDefaultAction
    return this
  }

  def ON_DELETE_NO_ACTION: this.type = onDeleteNoAction
  def ON_DELETE_CASCADE: this.type = onDeleteCascade
  def ON_DELETE_RESTRICT: this.type = onDeleteRestrict
  def ON_DELETE_SET_NULL: this.type = onDeleteSetNull
  def ON_DELETE_SET_DEFAULT: this.type = onDeleteSetDefault

  def ON_UPDATE_NO_ACTION: this.type = onUpdateNoAction
  def ON_UPDATE_CASCADE: this.type = onUpdateCascade
  def ON_UPDATE_RESTRICT: this.type = onUpdateRestrict
  def ON_UPDATE_SET_NULL: this.type = onUpdateSetNull
  def ON_UPDATE_SET_DEFAULT: this.type = onUpdateSetDefault

}
