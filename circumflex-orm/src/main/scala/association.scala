package ru.circumflex
package orm

import core._

/*!# Association

The `Association` class lets you create associations between relations which are
typically represented by foreign key constraints in database. This kind of
relationship is often referred to as _one-to-one_ or _many-to-one_ (the former
is implemented by adding a `UNIQUE` constraint).

We use some terminology when speaking about associations:

  * the `C` type parameter points to the relation which owns this association
  (we refer to it as the _child relation_);
  * the `P` type parameter points to the referenced relation (we refer to it as
  the _parent relation_);
  * the `K` type parameter is a type of this association field's value, it must
  match the type of parent relation's primary key.
*/
class Association[K, C <: Record[_, C], P <: Record[K, P]] (
        val field: Field[K, C], val parentRelation: Relation[K, P])
    extends ValueHolder[P, C] { assoc =>

  def name = field.name
  def record = field.record

  // Cascading actions

  protected var _onDelete: ForeignKeyAction = NO_ACTION
  def onDeleteClause = _onDelete
  def ON_DELETE(action: ForeignKeyAction): this.type = {
    _onDelete = action
    this
  }

  protected var _onUpdate: ForeignKeyAction = NO_ACTION
  def onUpdateClause = _onUpdate
  def ON_UPDATE(action: ForeignKeyAction): this.type = {
    _onUpdate = action
    this
  }

  // State maintenance

  override def value: Option[P] =
    field.value.flatMap(id => parentRelation.get(id))

  override def set(v: Option[P]): this.type = {
    field.set(v.flatMap(_.PRIMARY_KEY.value))
    this
  }

  // Simple expressions

  def IS(record: P): Predicate =
    new SimpleExpression(ormConf.dialect.EQ(aliasedName, placeholder), List(record.PRIMARY_KEY.get))
  def IS_NOT(record: P): Predicate =
    new SimpleExpression(ormConf.dialect.NE(aliasedName, placeholder), List(record.PRIMARY_KEY.get))
  def IN(params: Iterable[P]): Predicate = new SimpleExpression(
    ormConf.dialect.parameterizedIn(aliasedName, params.map(p => placeholder)),
    params.map(_.PRIMARY_KEY.get).toList)

  def joinPredicate(childAlias: String, parentAlias: String): Predicate = {
    val lh = ormConf.dialect.qualifyColumn(field, childAlias)
    val rh = ormConf.dialect.qualifyColumn(parentRelation.PRIMARY_KEY, parentAlias)
    new SimpleExpression(ormConf.dialect.EQ(lh, rh), Nil)
  }

}

/*!# Inverse Associations

Inverse assocations provide a way to access c hild records from parent relation.
This type of relationship is often referred to as _one-to-one_ or _one-to-many_
(the former one is implemented by applying a `UNIQUE` constraint).
They are essentially useful in a combination with `Criteria` for fetching
whole hierarchy of associated records in a single SQL `SELECT`.
*/
trait InverseAssociation[K, C <: Record[_, C], P <: Record[K, P], T]
    extends Wrapper[T] {
  def item: T = get
  def association: Association[K, C, P]
  def record: P
  def fetch(): Seq[C] = if (record.isTransient) Nil
  else tx.cache.cacheInverse(record.PRIMARY_KEY(), association, {
    val root = association.field.record.relation AS "root"
    aliasStack.push(root.alias)
    SELECT(root.*).FROM(root).WHERE(association.field EQ record.PRIMARY_KEY()).list()
  })
  def get: T
  def apply: T = get

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
  def get: Seq[C] = fetch()
}

class InverseOne[K, C <: Record[_, C], P <: Record[K, P]](
        val record: P, val association: Association[K, C, P])
    extends InverseAssociation[K, C, P, Option[C]] {
  def get: Option[C] = {
    val children = fetch()
    if (children.size <= 0) return None
    if (children.size > 1)
      throw new ORMException("One-to-one relationship expected, but multiple records found. " +
          "Add a UNIQUE constraint or stick with InverseMany.")
    Some(children(0))
  }
}