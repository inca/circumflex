package ru.circumflex.orm

import ORM._

/**
 * Represents SQL index.
 */
class Index[R](val relation: Relation[R],
               val indexName: String) extends SchemaObject {

  private var _expressions: Seq[String] = Nil
  private var _unique: Boolean = false;
  private var _method: String = "btree"
  private var _predicate: Predicate = EmptyPredicate

  /**
   * Returns expressions that define this index.
   */
  def expressions: Seq[String] = _expressions

  /**
   * Adds one or more expressions to index definition.
   */
  def add(expr: String*): this.type = {
    _expressions ++= expr.toList
    return this
  }

  /**
   * Adds one or more columns to index definition.
   */
  def add(col: Column[_, R]): this.type = {
    _expressions ++= List(col.columnName)
    return this
  }

  /**
   * Determines, whether the index is unique or not.
   */
  def unique_?(): Boolean = _unique

  /**
   * DSL-like way to define unique index.
   */
  def unique: this.type = {
    _unique = true
    return this
  }

  /**
   * Returns indexing method name.
   */
  def using: String = _method

  /**
   * DSL-like way to define indexing method.
   */
  def using(method: String): this.type = {
    _method = method
    return this
  }

  /**
   * Returns an indexing predicate.
   */
  def where = _predicate

  /**
   * DSL-like way to define indexing predicate.
   */
  def where(predicate: Predicate): this.type = {
    _predicate = predicate
    return this
  }

  def objectName = indexName

  def sqlCreate = dialect.createIndex(this)

  def sqlDrop = dialect.dropIndex(this)

}