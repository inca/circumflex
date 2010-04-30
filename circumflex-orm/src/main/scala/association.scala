package ru.circumflex.orm

import ORM._

// ## Association

class Association[R <: Record[R], F <: Record[F]](name: String,
                                                  uuid: String,
                                                  val record: R,
                                                  val foreignRelation: Relation[F])
    extends ValueHolder[F](name, uuid) { assoc =>

  // ### Commons

  class InternalField extends NotNullField[Long](name, uuid, dialect.longType) {
    override def setValue(newValue: Long): this.type = {
      super.setValue(newValue)
      assoc._value = null.asInstanceOf[F]
      return this
    }
  }

  val field = new InternalField()

  override def setValue(newValue: F): this.type = {
    newValue.id() match {
      case Some(id) =>
        field.setValue(id)
        super.setValue(newValue)
        return this
      case _ => throw new ORMException("Cannot assign transient record to association.")
    }
  }

  // ### Cascading actions for DDL

  protected var _onDelete: ForeignKeyAction = NO_ACTION
  protected var _onUpdate: ForeignKeyAction = NO_ACTION

  def onDelete = _onDelete
  def onUpdate = _onUpdate

  def onDelete(action: ForeignKeyAction): this.type = {
    _onDelete = action
    return this
  }
  def ON_DELETE(action: ForeignKeyAction): this.type = onDelete(action)

  def onUpdate(action: ForeignKeyAction): this.type = {
    _onUpdate = action
    return this
  }
  def ON_UPDATE(action: ForeignKeyAction): this.type = onUpdate(action)

}
