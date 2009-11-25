package ru.circumflex.orm

import collection.mutable.HashMap
import java.sql.PreparedStatement
import java.util.UUID

/**
 * Contains base functionality for objects that can be retrieved from and
 * persisted to database relations.
 * There's a couple of things one must know about records.
 * <ul>
 * <li>Each record instance "knows" about it's main relation through
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
abstract class Record[R] extends JDBCHelper {
  private val uuid = UUID.randomUUID.toString

  private val fieldsMap = HashMap[Column[_, R], Any]()
  private val manyToOneMap = HashMap[Association[R, _], Any]()
  private val oneToManyMap = HashMap[Association[_, R], Seq[Any]]()

  def relation: Relation[R]

  def primaryKey: Option[_] = fieldsMap.get(relation.primaryKey.column)

  def isIdentified = primaryKey != None

  /* FIELDS-RELATED STUFF */

  def field[T](col: Column[T, R]) = new Field(this, col)

  def getField[T](col: Column[T, R]): Option[T] =
    fieldsMap.get(col).asInstanceOf[Option[T]]

  def setField[T](col: Column[T, R], value: T): Unit =
    setField(col, Some(value))

  def setField[T](col: Column[T, R], value: Option[T]) = {
    value match {
      case Some(value) => fieldsMap += (col -> value)
      case _ => fieldsMap -= col
    }
    // invalidate association caches
    manyToOneMap.keys.filter(_.localColumn == col).foreach(manyToOneMap -= _)
    oneToManyMap.keys.filter(_.localColumn == col).foreach(oneToManyMap -= _)
  }

  /* ASSOCIATIONS-RELATED STUFF */

  def manyToOne[P](association: Association[R, P]) =
    new ManyToOne[R, P](this, association)

  def getManyToOne[P](a: Association[R, P]): Option[P] =
    manyToOneMap.get(a) match {
      case Some(value : P) => Some(value)   // parent is already in cache
      case _ => {                           // lazy-fetch a parent
        getField(a.localColumn) match {
          case Some(localVal) => a.fetchManyToOne(localVal) match {
            case Some(mto : P) =>
              manyToOneMap += (a -> mto)
              Some(mto)
            case _ => None
          }
          case None => None
        }
      }
    }

  /* PERSISTENCE-RELATED STUFF */

  def insert(): Int = {
    val conn = relation.configuration.connectionProvider.getConnection
    val sql = relation.dialect.insertRecord(this)
    sqlLog.debug(sql)
    auto(conn.prepareStatement(sql))(st => {
      setParams(st, relation.columns)
      return st.executeUpdate
    })
  }

  def update(): Int = {
    val conn = relation.configuration.connectionProvider.getConnection
    val sql = relation.dialect.updateRecord(this)
    sqlLog.debug(sql)
    auto(conn.prepareStatement(sql))(st => {
      setParams(st, relation.nonPKColumns)
      relation.configuration.typeConverter.write(
        st,
        primaryKey.get,
        relation.nonPKColumns.size + 1)
      return st.executeUpdate
    })
  }

  def save() = if (isIdentified) update()
  else {
    generateFields
    insert()
  }

  def delete(): Int = {
    val conn = relation.configuration.connectionProvider.getConnection
    val sql = relation.dialect.deleteRecord(this)
    sqlLog.debug(sql)
    auto(conn.prepareStatement(sql))(st => {
      relation.configuration.typeConverter.write(st, primaryKey.get, 1)
      return st.executeUpdate
    })
  }

  def generateFields(): Unit =
    relation.columns.flatMap(_.sequence).foreach(seq => {
      val nextval = seq.nextValue
      this.setField(seq.column, nextval)
    })

  private def setParams(st: PreparedStatement, cols: Seq[Column[_, R]]) =
    (0 until cols.size).foreach(ix => {
      val col = cols(ix)
      val value = this.getField(col) match {
        case Some(v) => v
        case _ => null
      }
      relation.configuration.typeConverter.write(st, value, ix + 1)
    })

  /* EQUALS BOILERPLATE */

  override def equals(obj: Any) = obj match {
    case r: Record[R] if (r.relation == this.relation) =>
      this.primaryKey.getOrElse(this.uuid) == r.primaryKey.getOrElse(r.uuid)
    case _ => false
  }

  override def hashCode = this.primaryKey.getOrElse(uuid).hashCode

}

class Field[T, R](val record: Record[R],
                  val column: Column[T, R]) {
  def get: Option[T] = record.getField(column)

  def set(value: T): Unit = record.setField(column, value)

  def setNull: Unit = record.setField(column, None)

  def <=(value: T): Unit = set(value)

  def :=(value: T): Unit = set(value)

  override def toString = get match {
    case Some(value) => value.toString
    case None => ""
  }
}

class ManyToOne[C, P](val record: Record[C],
                      val association: Association[C, P]) {

  def get: Option[P] = record.getManyToOne(association)

  override def toString = get match {
    case Some(value) => value.toString
    case None => ""
  }

}