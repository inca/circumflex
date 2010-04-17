package ru.circumflex.orm

import ORM._
import ru.circumflex.core.CircumflexUtil
import CircumflexUtil._
import java.lang.reflect.Method

/**
 * ## Column meta information
 */
class ColumnMeta[R <: Relation[R], T](val column: Column[R, T],
                                      val method: Method)
    extends SQLable {

  def inferredName = camelCaseToUnderscore(method.getName)
  val columnName: String = column.columnName match {
    case Some(n) => n
    case _ => inferredName
  }

  val sqlType = column.sqlType
  val nullable = column.isInstanceOf[NullableColumn[R, T]]
  val default = column.default

  def toSql = dialect.columnDefinition(this)
}

/**
 * ## Relation meta information
 */
class RelationMeta[R <: Relation[R]](val relation: Relation[R]) {

  val relationName: String = relation.relationName match {
    case Some(n) => n
    case _ => inferredName
  }
  def className = relation.getClass.getSimpleName
  def inferredName = camelCaseToUnderscore(className)

  val columns: Seq[ColumnMeta[R, _]] = findColumns(relation.getClass)
  protected def findColumns(cl: Class[_]): Seq[ColumnMeta[R, _]] =
    if (cl == classOf[Any]) return Nil
    else {
      val names = cl.getDeclaredFields
          .filter(f => classOf[Column[R, _]].isAssignableFrom(f.getType))
          .map(f => f.getName)
      return findColumns(cl.getSuperclass) ++
          names.map(n => cl.getMethod(n))
              .map(m => new ColumnMeta(m.invoke(relation).asInstanceOf[Column[R, _]], m))
    }

  override def equals(that: Any) = that match {
    case Some(m: RelationMeta[R]) if relationName.toLowerCase == m.relationName.toLowerCase => true
    case _ => false
  }

  override def hashCode = relationName.toLowerCase.hashCode

  override def toString = relationName
}