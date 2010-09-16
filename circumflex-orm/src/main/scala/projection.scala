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

  override def toString = toSql

}

trait AtomicProjection[T] extends Projection[T] {

  protected[orm] var alias: String = "this"

  def AS(alias: String): this.type = {
    this.alias = alias
    return this
  }

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
      .map(_.read(rs))
      .map(id => cacheService.cacheRecord(id.asInstanceOf[PK], node.relation, readRecord(rs)))

  protected def readRecord(rs: ResultSet): R = {
    val record: R = node.relation.recordClass.newInstance
    _fieldProjections.foreach { p =>
      node.relation.methodsMap(p.field).invoke(record) match {
        case f: Field[Any, R] => f.set(p.read(rs))
        case _ => throw new ORMException("Could not set a field " + p.field +
            " on record " + record.getClass + ".")
      }
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

case class Tuple2Projection[T1, T2] (
    _1: Projection[T1], _2: Projection[T2])
    extends CompositeProjection[(Option[T1], Option[T2])] {
  def subProjections = List[Projection[_]](_1, _2)
  def read(rs: ResultSet): Option[(Option[T1], Option[T2])] =
    Some((_1.read(rs), _2.read(rs)))
}

case class Tuple3Projection[T1, T2, T3] (
    _1: Projection[T1], _2: Projection[T2], _3: Projection[T3])
    extends CompositeProjection[(Option[T1], Option[T2], Option[T3])] {
  def subProjections = List[Projection[_]](_1, _2, _3)
  def read(rs: ResultSet): Option[(Option[T1], Option[T2], Option[T3])] =
    Some((_1.read(rs), _2.read(rs), _3.read(rs)))
}

case class Tuple4Projection[T1, T2, T3, T4] (
    _1: Projection[T1], _2: Projection[T2], _3: Projection[T3], _4: Projection[T4])
    extends CompositeProjection[(Option[T1], Option[T2], Option[T3], Option[T4])] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4)
  def read(rs: ResultSet): Option[(Option[T1], Option[T2], Option[T3], Option[T4])] =
    Some((_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs)))
}

case class Tuple5Projection[T1, T2, T3, T4, T5] (
    _1: Projection[T1], _2: Projection[T2], _3: Projection[T3], _4: Projection[T4], _5: Projection[T5])
    extends CompositeProjection[(Option[T1], Option[T2], Option[T3], Option[T4], Option[T5])] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4, _5)
  def read(rs: ResultSet): Option[(Option[T1], Option[T2], Option[T3], Option[T4], Option[T5])] =
    Some((_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs), _5.read(rs)))
}

case class Tuple6Projection[T1, T2, T3, T4, T5, T6] (
    _1: Projection[T1], _2: Projection[T2], _3: Projection[T3], _4: Projection[T4], _5: Projection[T5], _6: Projection[T6])
    extends CompositeProjection[(Option[T1], Option[T2], Option[T3], Option[T4], Option[T5], Option[T6])] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4, _5, _6)
  def read(rs: ResultSet): Option[(Option[T1], Option[T2], Option[T3], Option[T4], Option[T5], Option[T6])] =
    Some((_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs), _5.read(rs), _6.read(rs)))
}

case class Tuple7Projection[T1, T2, T3, T4, T5, T6, T7] (
    _1: Projection[T1], _2: Projection[T2], _3: Projection[T3], _4: Projection[T4], _5: Projection[T5], _6: Projection[T6], _7: Projection[T7])
    extends CompositeProjection[(Option[T1], Option[T2], Option[T3], Option[T4], Option[T5], Option[T6], Option[T7])] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4, _5, _6, _7)
  def read(rs: ResultSet): Option[(Option[T1], Option[T2], Option[T3], Option[T4], Option[T5], Option[T6], Option[T7])] =
    Some((_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs), _5.read(rs), _6.read(rs), _7.read(rs)))
}

case class Tuple8Projection[T1, T2, T3, T4, T5, T6, T7, T8] (
    _1: Projection[T1], _2: Projection[T2], _3: Projection[T3], _4: Projection[T4], _5: Projection[T5], _6: Projection[T6], _7: Projection[T7], _8: Projection[T8])
    extends CompositeProjection[(Option[T1], Option[T2], Option[T3], Option[T4], Option[T5], Option[T6], Option[T7], Option[T8])] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4, _5, _6, _7, _8)
  def read(rs: ResultSet): Option[(Option[T1], Option[T2], Option[T3], Option[T4], Option[T5], Option[T6], Option[T7], Option[T8])] =
    Some((_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs), _5.read(rs), _6.read(rs), _7.read(rs), _8.read(rs)))
}

case class Tuple9Projection[T1, T2, T3, T4, T5, T6, T7, T8, T9] (
    _1: Projection[T1], _2: Projection[T2], _3: Projection[T3], _4: Projection[T4], _5: Projection[T5], _6: Projection[T6], _7: Projection[T7], _8: Projection[T8], _9: Projection[T9])
    extends CompositeProjection[(Option[T1], Option[T2], Option[T3], Option[T4], Option[T5], Option[T6], Option[T7], Option[T8], Option[T9])] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4, _5, _6, _7, _8, _9)
  def read(rs: ResultSet): Option[(Option[T1], Option[T2], Option[T3], Option[T4], Option[T5], Option[T6], Option[T7], Option[T8], Option[T9])] =
    Some((_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs), _5.read(rs), _6.read(rs), _7.read(rs), _8.read(rs), _9.read(rs)))
}

case class Tuple10Projection[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] (
    _1: Projection[T1], _2: Projection[T2], _3: Projection[T3], _4: Projection[T4], _5: Projection[T5], _6: Projection[T6], _7: Projection[T7], _8: Projection[T8], _9: Projection[T9], _10: Projection[T10])
    extends CompositeProjection[(Option[T1], Option[T2], Option[T3], Option[T4], Option[T5], Option[T6], Option[T7], Option[T8], Option[T9], Option[T10])] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)
  def read(rs: ResultSet): Option[(Option[T1], Option[T2], Option[T3], Option[T4], Option[T5], Option[T6], Option[T7], Option[T8], Option[T9], Option[T10])] =
    Some((_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs), _5.read(rs), _6.read(rs), _7.read(rs), _8.read(rs), _9.read(rs), _10.read(rs)))
}
