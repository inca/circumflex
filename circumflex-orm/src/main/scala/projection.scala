package ru.circumflex.orm

import java.sql.ResultSet

/*!# Projections

In relational algebra a *projection* is a function which describes a subset of
columns returned from an SQL query. In Circumflex ORM instances of the `Projection`
trait are used to process `ResultSet` and determine the result type of SQL queries.

We distinguish between *atomic* and *composite* projections: the former ones
span across only one column of `ResultSet`, the latter ones contain a list of internal
projections and therefore span across multiple columns.

Like with relation nodes, special alias `this` is expanded into query-unique alias
to prevent collisions when aliases are not assigned explicitly.

Circumflex ORM supports querying arbitrary expressions which your database understands,
you only need to explicitly specify an expected type.
*/
trait Projection[T] extends SQLable {

  def read(rs: ResultSet): Option[T]

  def sqlAliases: Seq[String]

  protected[orm] var alias: String = "this"

  def AS(alias: String): this.type = {
    this.alias = alias
    return this
  }

  override def toString = toSql

}

trait AtomicProjection[T] extends Projection[T] {

  def sqlAliases = List(alias)

}

trait CompositeProjection[T] extends Projection[T] {

  def subProjections: Seq[Projection[_]]

  def sqlAliases = subProjections.flatMap(_.sqlAliases)

  override def equals(obj: Any) = obj match {
    case p: CompositeProjection[_] =>
      this.subProjections.toList == p.subProjections.toList
    case _ => false
  }

  private var _hash = 0;
  override def hashCode: Int = {
    if (_hash == 0)
      for (p <- subProjections)
        _hash = 31 * _hash + p.hashCode
    return _hash
  }

  def toSql = subProjections.map(_.toSql).mkString(", ")

}

class ExpressionProjection[T](val expression: String)
    extends AtomicProjection[T] {

  def toSql = dialect.alias(expression, alias)

  def read(rs: ResultSet) = typeConverter.read(rs, alias).asInstanceOf[Option[T]]

  override def equals(obj: Any) = obj match {
    case p: ExpressionProjection[_] =>
      p.expression == this.expression
    case _ => false
  }

  override def hashCode = expression.hashCode
}

class FieldProjection[T, R <: Record[_, R]](val node: RelationNode[_, R],
                                            val field: Field[T, R])
    extends AtomicProjection[T] {

  def expr = dialect.qualifyColumn(field, node.alias)

  def toSql = dialect.alias(expr, alias)

  def read(rs: ResultSet) = field.read(rs, alias)

  override def equals(obj: Any) = obj match {
    case p: FieldProjection[_, _] =>
      p.node == this.node && p.field.name == this.field.name
    case _ => false
  }

  override def hashCode = node.hashCode * 31 + field.name.hashCode
}

class RecordProjection[PK, R <: Record[PK, R]](val node: RelationNode[PK, R])
    extends CompositeProjection[R] {

  protected val _fieldProjections: Seq[FieldProjection[_, R]] = node
      .relation.fields.map(f => new FieldProjection(node, f))

  def subProjections = _fieldProjections

  def read(rs: ResultSet): Option[R] = _fieldProjections
      .find(_.field == node.relation.PRIMARY_KEY)
      .flatMap(_.read(rs))
      .flatMap(id => contextCache.cacheRecord(id.asInstanceOf[PK], node.relation, Some(readRecord(rs))))

  protected def readRecord(rs: ResultSet): R = {
    val record: R = node.relation.recordClass.newInstance
    _fieldProjections.foreach { p =>
      node.relation.getField(record, p.field.asInstanceOf[Field[Any, R]]).set(p.read(rs))
    }
    return record
  }

  override def equals(obj: Any) = obj match {
    case p: RecordProjection[_, _] => this.node == p.node
    case _ => false
  }

  override def hashCode = node.hashCode

}

class UntypedTupleProjection(val subProjections: Projection[_]*)
    extends CompositeProjection[Array[Option[Any]]] {
  def read(rs: ResultSet): Option[Array[Option[Any]]] = Some(subProjections.map(_.read(rs)).toArray)
}

case class PairProjection[T1, T2] (
    _1: Projection[T1], _2: Projection[T2])
    extends CompositeProjection[(Option[T1], Option[T2])] {
  def subProjections = List[Projection[_]](_1, _2)
  def read(rs: ResultSet): Option[(Option[T1], Option[T2])] =
    Some((_1.read(rs), _2.read(rs)))
}
