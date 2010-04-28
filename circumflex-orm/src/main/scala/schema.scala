package ru.circumflex.orm

import ORM._

// ## Schema Objects for DDL

// ### Constraints

/**
 * Common stuff for all constraints.
 */
abstract class Constraint(val relation: Relation[_],
                          val constraintName: String)
    extends SchemaObject with SQLable {

  def objectName = constraintName

  def sqlCreate = dialect.alterTableAddConstraint(this)
  def sqlDrop = dialect.alterTableDropConstraint(this)

  def toSql = dialect.constraintDefinition(this)

  def sqlDefinition: String

  override def toString = toSql
}

/**
 * An SQL `UNIQUE` constraint.
 */
class UniqueKey(relation: Relation[_],
                name: String,
                val fields: Seq[Field[_]])
    extends Constraint(relation, name) {
  def sqlDefinition = dialect.uniqueKeyDefinition(this)
}

/**
 * An SQL `FOREIGN KEY` constraint.
 */
class ForeignKey(relation: Relation[_],
                 name: String,
                 val foreignRelation: Relation[_],
                 val localFields: Seq[Field[_]],
                 val foreignFields: Seq[Field[_]],
                 protected var _onDelete: ForeignKeyAction,
                 protected var _onUpdate: ForeignKeyAction)
    extends Constraint(relation, name) {

  def onDelete = _onDelete
  def onDelete(action: ForeignKeyAction): this.type = {
    _onDelete = action
    return this
  }
  def ON_DELETE(action: ForeignKeyAction): this.type = onDelete(action)

  def onUpdate = _onUpdate
  def onUpdate(action: ForeignKeyAction): this.type = {
    _onUpdate = action
    return this
  }
  def ON_UPDATE(action: ForeignKeyAction): this.type = onUpdate(action)

  def sqlDefinition = dialect.foreignKeyDefinition(this)
}

/**
 * An SQL `FOREIGN KEY` constraint.
 */
class CheckConstraint(relation: Relation[_],
                      name: String,
                      val expression: String)
    extends Constraint(relation, name) {
  def sqlDefinition = dialect.checkConstraintDefinition(this)
}

/**
 * A helper to create SQL constraints for the relation.
 */
class ConstraintHelper(relation: Relation[_], name: String) {
  def unique(fields: Field[_]*): UniqueKey = {
    val uniq = new UniqueKey(relation, name, fields.toList)
    relation._constraints ++= List(uniq)
    return uniq
  }
  def UNIQUE(fields: Field[_]*) = unique(fields: _*)

  def check(expression: String): CheckConstraint = {
    val chk = new CheckConstraint(relation, name, expression)
    relation._constraints ++= List(chk)
    return chk
  }
  def CHECK(expression: String) = check(expression: String)

  def foreignKey(foreignRelation: Relation[_],
                 localFields: Seq[Field[_]],
                 foreignFields: Seq[Field[_]]): ForeignKey = {
    val fk = new ForeignKey(relation, name, foreignRelation, localFields, foreignFields,
      NO_ACTION, NO_ACTION)
    relation._constraints ++= List(fk)
    return fk
  }
  def FOREIGN_KEY(foreignRelation: Relation[_],
                  localFields: Seq[Field[_]],
                  foreignFields: Seq[Field[_]]): ForeignKey =
    foreignKey(foreignRelation, localFields, foreignFields)

  def foreignKey(foreignRelation: Relation[_],
                 fields: Pair[Field[_], Field[_]]*): ForeignKey = {
    val localFileds = fields.map(_._1)
    val foreignFields = fields.map(_._2)
    return foreignKey(foreignRelation, localFileds, foreignFields)
  }
  def FOREIGN_KEY(foreignRelation: Relation[_],
                  fields: Pair[Field[_], Field[_]]*): ForeignKey =
    foreignKey(foreignRelation, fields: _*)

  def foreignKey(localFields: Field[_]*): ForeignKeyHelper =
    new ForeignKeyHelper(relation, name, localFields)
  def FOREIGN_KEY(localFields: Field[_]*): ForeignKeyHelper =
    foreignKey(localFields: _*)
}

/**
 * A special helper for creating foreign keys in DSL style.
 */
class ForeignKeyHelper(relation: Relation[_], name: String, localFields: Seq[Field[_]]) {
  def references(foreignRelation: Relation[_],
                 foreignFields: Field[_]*): ForeignKey = {
    val fk = new ForeignKey(relation, name, foreignRelation, localFields, foreignFields,
      NO_ACTION, NO_ACTION)
    relation._constraints ++= List(fk)
    return fk
  }
  def REFERENCES(foreignRelation: Relation[_],
                 foreignFields: Field[_]*): ForeignKey =
    references(foreignRelation, foreignFields: _*)
}

// ### Indexes

/**
 * Index definition for the relation.
 */
class Index(val relation: Relation[_],
            val name: String,
            val expression: String) 
    extends SchemaObject {

  /**
   * DSL for defining `UNIQUE` indexes.
   */  
  protected var _unique: Boolean = false
  def unique_?() = _unique
  def unique: this.type = {
    this._unique = true
    return this
  }
  def UNIQUE: this.type = unique

  /**
   * DSL for defining indexing method.
   */
  private var _method: String = "btree"
  def using = _method
  def using(method: String): this.type = {
    this._method = method
    return this
  }
  def USING(method: String): this.type = using(method)

  /**
   * DSL for defining indexing predicate.
   */
  private var _predicate: Predicate = EmptyPredicate
  def where = _predicate
  def where(predicate: Predicate): this.type = {
    this._predicate = predicate
    return this
  }
  def WHERE(predicate: Predicate): this.type = where(predicate)

  def objectName = name
  def sqlCreate = dialect.createIndex(this)
  def sqlDrop = dialect.dropIndex(this)
}
