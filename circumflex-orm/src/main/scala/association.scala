package ru.circumflex.orm

/**
 * Defines a contract for parent-child (one-to-many) associations.
 * Usually they are modelled as tables with foreign keys.
 */
trait Association[C, P] {

  import Query._

  /**
   * Returns child relation.
   */
  def childRelation: Relation[C]

  /**
   * Returns parent relation.
   */
  def parentRelation: Relation[P]

  /**
   * Returns a list of local columns.
   */
  def localColumn: Column[_, C]

  /**
   * Returns a column of the referenced table (parent) that matches local column.
   */
  def referenceColumn: Column[_, P] = parentRelation.primaryKey.column

  protected def manyToOneQuery(localValue: Any): Select = {
    val q = select()
    val node = q.makeNode(parentRelation)
    q.addFrom(node)
    q.where(node.field(referenceColumn).eq(localValue))
    return q
  }

  def fetchManyToOne(localValue: Any): Option[P] =
    manyToOneQuery(localValue).unique.map(_.apply(0)).asInstanceOf[Option[P]]

  protected def oneToManyQuery(referenceValue: Any): Select = {
    val q = select()
    val node = q.makeNode(childRelation)
    q.addFrom(node)
    q.where(node.field(localColumn).eq(referenceValue))
    return q
  }

  def fetchOneToMany(referenceValue: Any): Seq[C] =
    oneToManyQuery(referenceValue).list.asInstanceOf[Seq[C]]

}
