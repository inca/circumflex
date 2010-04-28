package ru.circumflex.orm

import ORM._
import java.sql.ResultSet

// ## Projection Basics

trait Projection[T] extends SQLable {

  /**
   * Extract a value from result set.
   */
  def read(rs: ResultSet): T

  /**
   * Returns the list of aliases, from which this projection is composed.
   */
  def sqlAliases: Seq[String]

  override def toString = toSql
}

/**
 * A contract for single-column projections with assigned aliases
 * so that they may be used in ORDER BY/HAVING clauses.
 */
trait AtomicProjection[T] extends Projection[T] {
  /**
   * Projection's alias (`this` is expanded to query-unique alias).
   */
  protected[orm] var alias: String = "this"

  def read(rs: ResultSet) = typeConverter.read(rs, alias).asInstanceOf[T]

  /**
   * Change an alias of this projection.
   */
  def as(alias: String): this.type = {
    this.alias = alias
    return this
  }

  def sqlAliases = List(alias)
}

/**
 * A contract for multi-column projections that should be read as a composite object.
 */
trait CompositeProjection[R] extends Projection[R] {

  private var _hash = 0;

  def subProjections: Seq[Projection[_]]

  def sqlAliases = subProjections.flatMap(_.sqlAliases)

  override def equals(obj: Any) = obj match {
    case p: CompositeProjection[R] =>
      (this.subProjections.toList -- p.subProjections.toList) == Nil
    case _ => false
  }

  override def hashCode: Int = {
    if (_hash == 0)
      for (p <- subProjections)
        _hash = 31 * _hash + p.hashCode
    return _hash
  }

  def toSql = subProjections.map(_.toSql).mkString(", ")

}

// ## Expression Projection

/**
 * This projection represents an arbitrary expression that RDBMS can understand
 * within `SELECT` clause (for example, `current_timestamp` or `count(*)`).
 */
class ExpressionProjection[T](val expression: String)
    extends AtomicProjection[T] {

  def toSql = dialect.alias(expression, alias)

  override def equals(obj: Any) = obj match {
    case p: ExpressionProjection[T] =>
      p.expression == this.expression
    case _ => false
  }

  override def hashCode = expression.hashCode
}

// ## Field Projection

/**
 * A projection for single field of a record.
 */
class FieldProjection[T, R <: Record[R]](val node: RelationNode[R],
                                         val field: Field[T])
    extends AtomicProjection[T] {

  /**
   * Returns a column name qualified with node's alias.
   */
  def expr = dialect.qualifyColumn(field, node.alias)

  def toSql = dialect.alias(expr, alias)

  override def equals(obj: Any) = obj match {
    case p: FieldProjection[T, R] =>
      p.node == this.node && p.field.name == this.field.name
    case _ => false
  }

  override def hashCode = node.hashCode * 31 + field.name.hashCode
}

// ## Record Projection

/**
 * A projection for reading entire `Record`.
 */
class RecordProjection[R <: Record[R]](val node: RelationNode[R])
    extends CompositeProjection[R] {
  protected val _fieldProjections: Seq[FieldProjection[Any, R]] = node
      .relation
      .fields
      .map(f => new FieldProjection(node, f.asInstanceOf[Field[Any]]))
  def subProjections = _fieldProjections

  // We will return this `null`s in any failure conditions.
  protected def nope: R = null.asInstanceOf[R]

  def read(rs: ResultSet): R =
    _fieldProjections.find(_.field == node.relation.primaryKey) match {
      case Some(pkProjection) => pkProjection.read(rs) match {
        case null => nope
        case id => tx.getCachedRecord(node.relation, id) match {
          case Some(record: R) => record
          case _ => readRecord(rs)
        }
      } case _ => nope
    }

  protected def readRecord(rs: ResultSet): R = {
    val record: R = node.relation.recordClass.newInstance
    _fieldProjections.foreach(p => node.relation.methodsMap(p.field).invoke(record) match {
      case vh: ValueHolder[_] => setValue(vh, p.read(rs))
      case _ => throw new ORMException("Could not set a field " + p.field +
          " on record " + record.uuid + ".")
    })
    // if record remains unidentified, do not return it
    if (record.transient_?) return nope
    else {
      // otherwise cache it and return
      tx.updateRecordCache(record)
      return record
    }
  }

  protected def setValue(vh: ValueHolder[_], value: Any): Unit = vh match {
    case f: NullableField[Any] => f.setValue(Some(value))
    case f: NotNullField[Any] => f.setValue(value)
    case a: Association[_, _] => setValue(a.field, value)
    case _ =>
  }

  override def equals(obj: Any) = obj match {
    case p: RecordProjection[R] => this.node == p.node
    case _ => false
  }

  override def hashCode = node.hashCode

}

// ## Projections for tuples

class AnyTupleProjection(val subProjections: Projection[_]*)
    extends CompositeProjection[Array[Any]] {
  def read(rs: ResultSet): Array[Any] = subProjections.map(_.read(rs)).toArray
}

case class Tuple2Projection[T1, T2] (
        val _1: Projection[T1],
        val _2: Projection[T2]) 
        extends CompositeProjection[Tuple2[T1, T2]] {
  def subProjections = List[Projection[_]](_1, _2)
  def read(rs: ResultSet): Tuple2[T1, T2] =
      Tuple(_1.read(rs), _2.read(rs))
}

case class Tuple3Projection[T1, T2, T3] (
        val _1: Projection[T1],
        val _2: Projection[T2], 
        val _3: Projection[T3]) 
        extends CompositeProjection[Tuple3[T1, T2, T3]] {
  def subProjections = List[Projection[_]](_1, _2, _3)
  def read(rs: ResultSet): Tuple3[T1, T2, T3] =
      Tuple(_1.read(rs), _2.read(rs), _3.read(rs))
}

case class Tuple4Projection[T1, T2, T3, T4] (
        val _1: Projection[T1],
        val _2: Projection[T2], 
        val _3: Projection[T3], 
        val _4: Projection[T4]) 
        extends CompositeProjection[Tuple4[T1, T2, T3, T4]] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4)
  def read(rs: ResultSet): Tuple4[T1, T2, T3, T4] =
      Tuple(_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs))
}

case class Tuple5Projection[T1, T2, T3, T4, T5] (
        val _1: Projection[T1],
        val _2: Projection[T2], 
        val _3: Projection[T3], 
        val _4: Projection[T4], 
        val _5: Projection[T5]) 
        extends CompositeProjection[Tuple5[T1, T2, T3, T4, T5]] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4, _5)
  def read(rs: ResultSet): Tuple5[T1, T2, T3, T4, T5] =
      Tuple(_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs), _5.read(rs))
}

case class Tuple6Projection[T1, T2, T3, T4, T5, T6] (
        val _1: Projection[T1],
        val _2: Projection[T2], 
        val _3: Projection[T3], 
        val _4: Projection[T4], 
        val _5: Projection[T5], 
        val _6: Projection[T6]) 
        extends CompositeProjection[Tuple6[T1, T2, T3, T4, T5, T6]] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4, _5, _6)
  def read(rs: ResultSet): Tuple6[T1, T2, T3, T4, T5, T6] =
      Tuple(_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs), _5.read(rs),
          _6.read(rs))
}

case class Tuple7Projection[T1, T2, T3, T4, T5, T6, T7] (
        val _1: Projection[T1],
        val _2: Projection[T2], 
        val _3: Projection[T3], 
        val _4: Projection[T4], 
        val _5: Projection[T5], 
        val _6: Projection[T6], 
        val _7: Projection[T7]) 
        extends CompositeProjection[Tuple7[T1, T2, T3, T4, T5, T6, T7]] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4, _5, _6, _7)
  def read(rs: ResultSet): Tuple7[T1, T2, T3, T4, T5, T6, T7] =
      Tuple(_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs), _5.read(rs),
          _6.read(rs), _7.read(rs))
}

case class Tuple8Projection[T1, T2, T3, T4, T5, T6, T7, T8] (
        val _1: Projection[T1],
        val _2: Projection[T2], 
        val _3: Projection[T3], 
        val _4: Projection[T4], 
        val _5: Projection[T5], 
        val _6: Projection[T6], 
        val _7: Projection[T7], 
        val _8: Projection[T8]) 
        extends CompositeProjection[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4, _5, _6, _7, _8)
  def read(rs: ResultSet): Tuple8[T1, T2, T3, T4, T5, T6, T7, T8] =
      Tuple(_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs), _5.read(rs),
          _6.read(rs), _7.read(rs), _8.read(rs))
}

case class Tuple9Projection[T1, T2, T3, T4, T5, T6, T7, T8, T9] (
        val _1: Projection[T1],
        val _2: Projection[T2], 
        val _3: Projection[T3], 
        val _4: Projection[T4], 
        val _5: Projection[T5], 
        val _6: Projection[T6], 
        val _7: Projection[T7], 
        val _8: Projection[T8], 
        val _9: Projection[T9]) 
        extends CompositeProjection[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4, _5, _6, _7, _8, _9)
  def read(rs: ResultSet): Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =
      Tuple(_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs), _5.read(rs),
          _6.read(rs), _7.read(rs), _8.read(rs), _9.read(rs))
}

case class Tuple10Projection[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] (
        val _1: Projection[T1],
        val _2: Projection[T2], 
        val _3: Projection[T3], 
        val _4: Projection[T4], 
        val _5: Projection[T5], 
        val _6: Projection[T6], 
        val _7: Projection[T7], 
        val _8: Projection[T8], 
        val _9: Projection[T9], 
        val _10: Projection[T10]) 
        extends CompositeProjection[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] {
  def subProjections = List[Projection[_]](_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)
  def read(rs: ResultSet): Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
      Tuple(_1.read(rs), _2.read(rs), _3.read(rs), _4.read(rs), _5.read(rs),
          _6.read(rs), _7.read(rs), _8.read(rs), _9.read(rs), _10.read(rs))
}




