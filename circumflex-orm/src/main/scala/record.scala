package ru.circumflex.orm

import collection.mutable.HashMap
import java.util.UUID
import ORM._

/**
 * Contains base functionality for objects that can be retrieved from and
 * persisted to database relations.
 * There's a couple of things one must know about records.
 * <ul>
 * <li>Each record instance "knows" about it's relation through
 * <code>relation</code> method.</li>
 * <li>Records carry the data around using <em>fields</em; internally they are
 * stored in <code>fieldsMap</code> in a "column-to-value" form.</li>
 * <li>Each record has a primary key field which identifies the record in database.
 * The <code>identified_?</code> method determines, whether primary key field is set.</li>
 * <li>Two records are considered equal if their relations and primary key
 * fields are equal. If they are not identified, the internally generated uuid is
 * used for equality testing (so unidentified records never match each other).</li>
 * </ul>
 */
abstract class Record[R] {
  val internalUUID = UUID.randomUUID.toString
  val fieldsMap = new HashMap[Column[_, R], Any]()
  val relation: Relation[R] = ORMRegistry.getRelation(this)

  def primaryKey: Option[_] = fieldsMap.get(relation.primaryKey.column)
  def identified_?() = primaryKey != None

  /* FIELDS-RELATED STUFF */

  def field[T](col: Column[T, R]) = new ColumnField(this, col)
  def getField[T](col: Column[T, R]): Option[T] =
    fieldsMap.get(col).asInstanceOf[Option[T]]
  def setField[T](col: Column[T, R], value: T): Unit =
    setField(col, Some(value))
  def setField[T](col: Column[T, R], value: Option[T]) =
    value match {
      case Some(value) => fieldsMap += (col -> value)
      case _ => fieldsMap -= col
    }

  /* ASSOCIATIONS-RELATED STUFF */

  def manyToOne[P](association: Association[R, P]) =
    new ManyToOne[R, P](this, association)
  def oneToMany[C](association: Association[C, R]) =
    new OneToMany[C, R](this, association)
  def oneToOne[C](association: Association[C, R]) =
    new OneToOne[C, R](this, association)

  /* PERSISTENCE-RELATED STUFF */

  def validate(): Option[Seq[ValidationError]] = relation.validate(this)
  def validate_!(): Unit = relation.validate_!(this)
  def insert(): Int = relation.insert(this)
  def insert_!(): Int = relation.insert_!(this)
  def update(): Int = relation.update(this)
  def update_!(): Int = relation.update_!(this)
  def save(): Int = relation.save(this)
  def save_!(): Int = relation.save_!(this)
  def delete(): Int = relation.delete(this)

  /* EQUALITY AND OTHER STUFF */

  override def equals(obj: Any) = obj match {
    case r: Record[R] if (r.relation == this.relation) =>
      this.primaryKey.getOrElse(this.internalUUID) == r.primaryKey.getOrElse(r.internalUUID)
    case _ => false
  }
  override def hashCode = this.primaryKey.getOrElse(internalUUID).hashCode
  override def toString = relation.relationName + ": " + this.fieldsMap.toString
}

trait RecordScalar[R] {
  def record: Record[R]
}

trait RecordLocalAssociation[R, P] {
  def record: Record[R]
  def association: Association[R, P]
}

trait RecordForeignAssociation[C, R] {
  def record: Record[R]
  def association: Association[C, R]
}

trait Field[T] {
  def default(value: T): this.type = {
    set(value)
    return this
  }
  def get: Option[T]
  def getOrElse(defaultValue: T): T = get match {
    case Some(value) => value
    case _ => defaultValue
  }
  def set(value: T): Unit
  def setNull: Unit
  def <=(value: T): Unit = set(value)
  def :=(value: T): Unit = set(value)
  override def toString = get match {
    case Some(value) => value.toString
    case None => ""
  }
}

trait Collection[T] {
  def default(values: T*): this.type = {
    set(values.toList)
    return this
  }
  def get: Seq[T]
  def set(value: Seq[T]): Unit
  def <=(value: Seq[T]): Unit = set(value)
  def :=(value: Seq[T]): Unit = set(value)
  override def toString = get.toString
}


class ColumnField[T, R](val record: Record[R],
                        val column: Column[T, R])
        extends Field[T] with RecordScalar[R] {
  def get: Option[T] = record.getField(column)
  def set(value: T): Unit = record.setField(column, value)
  def setNull: Unit = record.setField(column, None)
}

class ManyToOne[C, P](val record: Record[C],
                      val association: Association[C, P])
        extends Field[P] with RecordLocalAssociation[C, P] {

  protected val r = record.asInstanceOf[C]

  def get: Option[P] = tx.getCachedMTO(association, r) match {
    case None => record.getField(association.childColumn) match {     // lazy-fetch a parent
      case Some(localVal) => association.fetchManyToOne(localVal) match {
        case Some(mto : P) =>
          tx.updateMTOCache(association, r, mto)
          Some(mto)
        case _ => None
      } case _ => None
    } case v => v
  }

  protected def setManyToOne[P](value: Option[P]): Unit = value match {
    case Some(value: P) =>
      tx.updateMTOCache(association.asInstanceOf[Association[C,Any]], r, value)
      record.setField(association.childColumn.asInstanceOf[Column[Any, C]], value.asInstanceOf[Record[P]].primaryKey)
    case None =>
      tx.evictMTO(association, r)
      record.setField(association.childColumn, None)
  }

  def set(value: P): Unit = setManyToOne(Some(value))
  def setNull: Unit = setManyToOne(None)

}

class OneToOne[C, P](val record: Record[P],
                     val association: Association[C, P])
        extends Field[C] with RecordForeignAssociation[C, P] {

  protected val r = record.asInstanceOf[P]

  def get: Option[C] = tx.getCachedOTM(association, r) match {
    case Some(seq: Seq[C]) if (seq.size > 0) => Some(seq(0))
    case None => record.primaryKey match {
      case Some(refVal) =>            // lazy-fetch
        val children = association.fetchOneToMany(refVal)
        tx.updateOTMCache(association, r, children)
        return this.get
      case _ => None
    }
    case _ => None
  }

  protected def setOneToOne[C](value: Option[C]): Unit = value match {
    case Some(value: C) =>
      tx.updateOTMCache(association.asInstanceOf[Association[Any,P]], r, List(value))
    case None =>
      tx.updateOTMCache(association.asInstanceOf[Association[Any,P]], r, Nil)
  }

  def set(value: C): Unit = setOneToOne(Some(value))
  def setNull: Unit = setOneToOne(None)

}

class OneToMany[C, P](val record: Record[P],
                      val association: Association[C, P])
        extends Collection[C] with RecordForeignAssociation[C,P] {

  val r = record.asInstanceOf[P]

  def get: Seq[C] = tx.getCachedOTM(association, r) match {
    case Some(seq: Seq[C]) => seq
    case None => record.primaryKey match {
      case Some(refVal) =>            // lazy-fetch children
        val children = association.fetchOneToMany(refVal)
        tx.updateOTMCache(association, r, children)
        children
      case _ => Nil
    }
  }

  protected def setOneToMany[C](value: Seq[C]): Unit =
    tx.updateOTMCache(association.asInstanceOf[Association[Any,P]], r, value)

  def set(value: Seq[C]): Unit = setOneToMany(value)

}