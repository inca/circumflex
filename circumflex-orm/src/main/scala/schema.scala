package ru.circumflex.orm

import ORM._
import ru.circumflex.core.CircumflexUtil._
import java.lang.reflect.Method

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
}

/**
 * An SQL `UNIQUE` constraint.
 */
class UniqueKey(r: Relation[_], n: String, val columns: Seq[Column])
    extends Constraint(r, n) {
  def sqlDefinition = dialect.uniqueKeyDefinition(this)
}

// ### Columns

class Column(val relation: Relation[_],
             val method: Method)
    extends SQLable {

  protected[orm] val fieldSample = method.invoke(relation.recordSample).asInstanceOf[Field[_, _]]
  val columnName = camelCaseToUnderscore(method.getName)
  val sqlType = fieldSample.sqlType
  val default = fieldSample.default
  def nullable_?(): Boolean = fieldSample.getClass.isInstanceOf[NullableField[_, _]]
  def unique_?(): Boolean = fieldSample.unique_?

  def toSql = dialect.columnDefinition(this)

}

// ### Foreign Keys

trait ForeignKeyAction

object NoAction extends ForeignKeyAction
object CascadeAction extends ForeignKeyAction
object RestrictAction extends ForeignKeyAction
object SetNullAction extends ForeignKeyAction
object SetDefaultAction extends ForeignKeyAction
