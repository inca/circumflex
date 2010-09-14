package ru.circumflex.orm

/*!# Association

The `Association` class lets you create associations between relations which are
typically represented by foreign key constraints in database. This kind of
relationship is often refered to as *one-to-one* or *many-to-one* (the former
is implemented by adding a `UNIQUE` constaint).

We use some terminology when speaking about associations:

  * the `C` type parameter points to the relation which owns this association
  (we refer to it as the *child relation*);
  * the `P` type parameter points to the referenced relation (we refer to it as
  the *parent relation*);
  * the `K` type parameter is a type of this association field's value, it must
  match the type of parent relation's primary key.
*/
class Association[K, C <: Record[_, C], P <: Record[K, P]](val field: Field[K, C],
                                                           val parentRelation: Relation[K, P])
    extends ValueHolder[P, C](field.name, field.record, field.sqlType) {

  /*! Column definition methods delegate to underlying field. */
  override def notNull_?(): Boolean = field.notNull_?
  override def NOT_NULL(): this.type = {
    field.NOT_NULL
    return this
  }
  override def unique_?(): Boolean = field.unique_?
  override def UNIQUE(): this.type = {
    field.UNIQUE
    return this
  }
  override def defaultExpression: Option[String] = field.defaultExpression
  override def DEFAULT(expr: String): this.type = {
    field.DEFAULT(expr)
    return this
  }

  // Cascading actions

  protected var _onDelete: ForeignKeyAction = NO_ACTION
  def onDelete = _onDelete
  def ON_DELETE(action: ForeignKeyAction): this.type = {
    _onDelete = action
    return this
  }

  protected var _onUpdate: ForeignKeyAction = NO_ACTION
  def onUpdate = _onUpdate
  def ON_UPDATE(action: ForeignKeyAction): this.type = {
    _onUpdate = action
    return this
  }


}