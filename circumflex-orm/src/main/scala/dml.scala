package ru.circumflex.orm

import collection.mutable.ListBuffer
import java.sql.PreparedStatement
import ORM._

trait DMLQuery extends Query {

  /**
   * Executes a query, returns number of affected rows.
   */
  def executeUpdate: Int = transactionManager.dml(conn => {
    val sql = toSql
    sqlLog.debug(sql)
    auto(conn.prepareStatement(sql))(st => {
      setParams(st, 1)
      return st.executeUpdate
    })
  })

}

class NativeDMLQuery(fragment: SQLFragment) extends DMLQuery {
  def parameters = fragment.parameters
  def toSql = fragment.toSql
}

/**
 * Contains functionality for INSERT-SELECT operations.
 * The query must be prepared first with projections matching target relation's
 * columns.
 */
class InsertSelect[R](val relation: Relation[R], val query: Select)
    extends DMLQuery {

  if (relation.readOnly)
      throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")

  def parameters = query.parameters

  def toSql: String = dialect.insertSelect(this)
}

/**
 * Contains functionality for DELETE operations. 
 */
class Delete[R](val relation: RelationNode[R])
    extends DMLQuery {

  if (relation.readOnly)
      throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")

  private var _predicate: Predicate = EmptyPredicate

  def parameters = _predicate.parameters

  /**
   * Sets WHERE clause of this query.
   */
  def where(predicate: Predicate): this.type = {
    this._predicate = predicate
    return this
  }

  /**
   * Returns the WHERE clause of this query.
   */
  def where: Predicate = this._predicate

  def toSql: String = dialect.delete(this)

}

/**
 * Contains functionality for UPDATE operations.
 */
class Update[R](val relation: Relation[R])
    extends DMLQuery {

  if (relation.readOnly)
      throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")

  private var _predicate: Predicate = EmptyPredicate
  private val _setClause = new ListBuffer[Pair[Column[_, R],Any]]()

  def parameters = _setClause.map(_._2) ++ _predicate.parameters

  /**
   * Sets WHERE clause of this query.
   */
  def where(predicate: Predicate): this.type = {
    this._predicate = predicate
    return this
  }

  /**
   * Returns the WHERE clause of this query.
   */
  def where: Predicate = this._predicate

  /**
   * Adds column-value pair to SET clause.
   */
  def set[T](column: Column[T, R], value: T): this.type = {
    _setClause += (column -> value)
    return this
  }

  /**
   * Adds column-value pair to SET clause for parent association
   * (assuming association;s local column and value's id).
   */
  def set[P](association: Association[R, P], value: Record[P]): this.type = {
    _setClause += (association.childColumn -> value.primaryKey.get)
    return this
  }

  /**
   * Adds column-NULL pair to SET clause.
   */
  def setNull(column: Column[_, R]): this.type = {
    _setClause += (column.asInstanceOf[Column[Any, R]] -> null)
    return this
  }

  /**
   * Adds column-NULL pair to SET clause for parent association.
   */
  def setNull(association: Association[R, _]): this.type =
    setNull(association.childColumn)

  /**
   * Returns the SET clause of this query.
   */
  def setClause = _setClause

  def toSql: String = dialect.update(this)

}