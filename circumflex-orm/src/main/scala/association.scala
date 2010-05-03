package ru.circumflex.orm

import ORM._

// ## Association

class Association[R <: Record[R], F <: Record[F]](name: String,
                                                  uuid: String,
                                                  val record: R,
                                                  val foreignRelation: Relation[F])
    extends ValueHolder[F](name, uuid) { assoc =>

  protected var _initialized: Boolean = false

  // ### Commons

  class InternalField extends Field[Long](name, uuid, dialect.longType) {
    override def setValue(newValue: Long): this.type = {
      super.setValue(newValue)
      assoc._value = null.asInstanceOf[F]
      assoc._initialized = false
      return this
    }
  }

  val field = new InternalField()

  override def getValue(): F = super.getValue() match {
    case null if (!_initialized && field() != None) =>
      _initialized = true
      // try to get from record cache
      val id = field.get
      tx.getCachedRecord(foreignRelation, id) match {
        case Some(record) => return record
        case None =>    // try to fetch lazily
          val r = foreignRelation.as("root")
          _value = (SELECT (r.*) FROM (r) WHERE (r.id EQ id))
              .unique
              .getOrElse(null.asInstanceOf[F])
          return _value
      }
    case value => return value
  }

  override def setValue(newValue: F): this.type = if (newValue == null) {
    field.setValue(null.asInstanceOf[Long])
    assoc._initialized = false
    super.setValue(null.asInstanceOf[F])
  } else newValue.id() match {
    case None => throw new ORMException("Cannot assign transient record to association.")
    case Some(id: Long) =>
      field.setValue(id)
      _initialized = true
      super.setValue(newValue)
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

// ## Inverse Associations

class InverseAssociation[P <: Record[P], C <: Record[C]](val record: P,
                                                         val association: Association[C, P]) {
  def getValue(): Seq[C] =
    if (record.transient_?) Nil
    else tx.getCachedInverse(this) match {  // lookup in cache
      case Nil => // lazy fetch
        val root = association.record.relation as "root"
        lastAlias(root.alias)
        val v = (SELECT (root.*) FROM (root) WHERE (association.field EQ record.id.get)).list
        tx.updateInverseCache(this, v)
        return v
      case l: Seq[C] =>
        return l
    }

  def apply(): Seq[C] = getValue()

}
