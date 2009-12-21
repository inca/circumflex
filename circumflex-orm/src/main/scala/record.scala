/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

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
 * The <code>isIdentified</code> method determines, whether primary key field is set.</li>
 * <li>Two records are considered equal if their relations and primary key
 * fields are equal. If they are not identified, the internally generated uuid is
 * used for equality testing (so unidentified records never match each other).</li>
 * </ul>
 */
abstract class Record[R] {
  private val uuid = UUID.randomUUID.toString

  val fieldsMap = new HashMap[Column[_, R], Any]()
  val manyToOneMap = new HashMap[Association[R, _], Any]()
  val oneToManyMap = new HashMap[Association[_, R], Seq[Any]]()

  def relation: Relation[R]

  def primaryKey: Option[_] = fieldsMap.get(relation.primaryKey.column)

  def isIdentified = primaryKey != None

  /* FIELDS-RELATED STUFF */

  def field[T](col: Column[T, R]) = new ColumnField(this, col)

  def getField[T](col: Column[T, R]): Option[T] =
    fieldsMap.get(col).asInstanceOf[Option[T]]

  def setField[T](col: Column[T, R], value: T): Unit =
    setField(col, Some(value))

  def setField[T](col: Column[T, R], value: Option[T]) = {
    value match {
      case Some(value) => fieldsMap += (col -> value)
      case _ => fieldsMap -= col
    }
    // invalidate associated many-to-one caches
    manyToOneMap.keys.filter(_.childColumn == col).foreach(manyToOneMap -= _)
    // invalidate one-to-many caches if identifier changed
    if (col == relation.primaryKey.column) oneToManyMap.clear
  }

  /* INLINE RECORD PROXY */

  def proxy[I](inlineRecord: ViewInlineRecord[I, R]) =
    new InlineRecordProxy(this, inlineRecord)

  /* ASSOCIATIONS-RELATED STUFF */

  def manyToOne[P](association: Association[R, P]) =
    new ManyToOne[R, P](this, association)

  def getManyToOne[P](a: Association[R, P]): Option[P] =
    manyToOneMap.get(a) match {
      case Some(value : P) => Some(value)   // parent is already in cache
      case _ => {
        getField(a.childColumn) match {     // lazy-fetch a parent
          case Some(localVal) => a.fetchManyToOne(localVal) match {
            case Some(mto : P) =>
              manyToOneMap += (a -> mto)
              Some(mto)
            case _ => None
          } case _ => None
        }
      }
    }

  def setManyToOne[P](a: Association[R, P], value: P): Unit =
    setManyToOne(a, Some(value))

  def setManyToOne[P](a: Association[R, P], value: Option[P]): Unit = value match {
    case Some(value: P) => {
      manyToOneMap += (a -> value)
      setField(a.childColumn.asInstanceOf[Column[Any, R]], value.asInstanceOf[Record[P]].primaryKey)
    }
    case None => {
      manyToOneMap -= a
      setField(a.childColumn, None)
    }
  }

  def oneToMany[C](association: Association[C, R]) =
    new OneToMany[C, R](this, association)

  def getOneToMany[C](a: Association[C, R]): Seq[C] =
    oneToManyMap.get(a) match {
      case None => primaryKey match {     // no cached children yet
        case Some(refVal) => {            // lazy-fetch children if identified
          val children = a.fetchOneToMany(refVal)
          oneToManyMap += (a -> children)
          children
        }
        case _ => Nil
      }
      case Some(seq: Seq[C]) => seq   // children are already in cache
    }

  def setOneToMany[C](a: Association[C, R], value: Seq[C]): Unit = {
    oneToManyMap += (a -> value)
  }

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

  def generateFields(): Unit = relation.generateFields(this)

  /* EQUALITY AND OTHER STUFF */

  override def equals(obj: Any) = obj match {
    case r: Record[R] if (r.relation == this.relation) =>
      this.primaryKey.getOrElse(this.uuid) == r.primaryKey.getOrElse(r.uuid)
    case _ => false
  }

  override def hashCode = this.primaryKey.getOrElse(uuid).hashCode

  override def toString = relation.relationName + ": " + this.fieldsMap.toString
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

  def setNull: Unit

  def <=(value: Seq[T]): Unit = set(value)

  def :=(value: Seq[T]): Unit = set(value)

  override def toString = get.toString
}


class ColumnField[T, R](val record: Record[R],
                        val column: Column[T, R])
        extends Field[T] {

  def get: Option[T] = record.getField(column)
  def set(value: T): Unit = record.setField(column, value)
  def setNull: Unit = record.setField(column, None)
}

class ManyToOne[C, P](val record: Record[C],
                      val association: Association[C, P])
        extends Field[P] {

  def get: Option[P] = record.getManyToOne(association)
  def set(value: P): Unit = record.setManyToOne(association, value)
  def setNull: Unit = record.setManyToOne(association, None)

}

class OneToMany[C, P](val record: Record[P],
                      val association: Association[C, P])
        extends Collection[C] {

  def get: Seq[C] = record.getOneToMany(association)
  def set(value: Seq[C]): Unit = record.setOneToMany(association, value)
  def setNull: Unit = record.setOneToMany(association, Nil)

}