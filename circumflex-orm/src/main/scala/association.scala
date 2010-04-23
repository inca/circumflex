package ru.circumflex.orm

import ORM._

// ## Association

abstract class Assocation[R <: Record[R], F <: Record[F]](val name: String,
                                                          val record: R,
                                                          val foreignRelation: Relation[F])
    extends ValueHolder[F] {

  // ### Commons


  // ### Cascading actions for DDL

  protected var _onDelete: ForeignKeyAction = NO_ACTION
  protected var _onUpdate: ForeignKeyAction = NO_ACTION

  def onDelete = _onDelete
  def onUpdate = _onUpdate

  def onDelete(action: ForeignKeyAction): this.type = {
    _onDelete = action
    return this
  }

  def onUpdate(action: ForeignKeyAction): this.type = {
    _onUpdate = action
    return this
  }

  def ON_DELETE(action: ForeignKeyAction): this.type = onDelete(action)
  def ON_UPDATE(action: ForeignKeyAction): this.type = onUpdate(action)

}

class NotNullAssociation[R <: Record[R], F <: Record[F]](name: String,
                                                         record: R,
                                                         foreignRelation: Relation[F])
    extends Assocation[R, F](name, record, foreignRelation)
