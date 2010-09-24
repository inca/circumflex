package ru.circumflex.orm

import ru.circumflex.core._

/*!# Association

The `Association` class lets you create associations between relations which are
typically represented by foreign key constraints in database. This kind of
relationship is often referred to as *one-to-one* or *many-to-one* (the former
is implemented by adding a `UNIQUE` constraint).

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
    extends ValueHolder[P, C](field.name, field.record, field.sqlType) { assoc =>

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

  // State maintenance

  override def value: Option[P] =
    field.value.flatMap(id => parentRelation.get(id))

  override def set(v: Option[P]): this.type = {
    field.set(v.flatMap(_.PRIMARY_KEY.value))
    return this
  }

}

/*!# Inverse Associations

Inverse assocations provide a way to access child records from parent relation.
This type of relationship is often referred to as *one-to-one* or *one-to-many*
(the former one is implemented by applying a `UNIQUE` constraint).
They are essentially useful in a combination with `Criteria` for fetching
whole hierarchy of associated records in a single SQL `SELECT`.
*/
trait InverseAssociation[K, C <: Record[_, C], P <: Record[K, P], T] {
  def association: Association[K, C, P]
  def record: P
  def fetch(): Seq[C] = if (record.transient_?) Nil
  else contextCache.cacheInverse(record.PRIMARY_KEY(), association, {
    val root = association.field.record.relation AS "root"
    ctx("orm.lastAlias") = root.alias
    SELECT(root.*).FROM(root).WHERE(association.field EQ record.PRIMARY_KEY()).list
  })
  def get(): T
  def apply(): T = get()

  override def equals(that: Any): Boolean = that match {
    case that: InverseAssociation[_, _, _, _] =>
      that.association == this.association
    case _ => false
  }
  override def hashCode: Int = association.hashCode
}

class InverseMany[K, C <: Record[_, C], P <: Record[K, P]](
    val record: P, val association: Association[K, C, P])
    extends InverseAssociation[K, C, P, Seq[C]] {
  def get(): Seq[C] = fetch()
}

class InverseOne[K, C <: Record[_, C], P <: Record[K, P]](
    val record: P, val association: Association[K, C, P])
    extends InverseAssociation[K, C, P, Option[C]] {
  def get(): Option[C] = {
    val children = fetch()
    if (children.size <= 0) return None
    if (children.size > 1)
      throw new ORMException("One-to-one relationship expected, by multiple records found. " +
          "Add a UNIQUE constraint or stick with InverseMany.")
    return Some(children(0))
  }
}