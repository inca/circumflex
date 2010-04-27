package ru.circumflex.orm

import ORM._
import java.sql.ResultSet
import collection.mutable.ListBuffer

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
    if (node.relation.virtual_?) {
      // skip caches and read record
      readRecord(rs)
    } else {
      val pk = node.relation.primaryKey
      _fieldProjections.find(_.field == pk) match {
        case Some(pkProjection) => pkProjection.read(rs) match {
          case null => nope
          case id => tx.getCachedRecord(node.relation, id) match {
            case Some(record: R) => record
            case _ => readRecord(rs)
          }
        } case _ => nope
      }
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
