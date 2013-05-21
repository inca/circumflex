package circumflex
package orm

import core._
import collection.mutable.HashMap
import java.util.Date

/*!# SQLable & Expression

Every object capable of rendering itself into an SQL statement
should extend the `SQLable` trait.*/
trait SQLable {
  def toSql: String
}

/*!# Parameterized expressions

The `Expression` trait provides basic functionality for dealing
with SQL expressions with JDBC-style parameters.
*/
trait Expression extends SQLable {

  def parameters: Seq[Any]

  def toInlineSql: String = parameters.foldLeft(toSql)((sql, p) =>
    sql.replaceFirst("\\?", ormConf.dialect.escapeParameter(p)
        .replaceAll("\\\\", "\\\\\\\\")
        .replaceAll("\\$", "\\\\\\$")))

  override def equals(that: Any) = that match {
    case e: Expression =>
      e.toSql == this.toSql && e.parameters.toList == this.parameters.toList
    case _ => false
  }

  override def hashCode = 0

  override def toString = toSql
}

object Expression {

  implicit def toPredicate(expression: Expression): Predicate =
      new SimpleExpression(expression.toSql, expression.parameters)

  implicit def toProjection[T](expression: Expression): Projection[T] =
    new ExpressionProjection[T](expression.toSql)

  implicit def toOrder(expression: Expression): Order =
    new Order(expression.toSql, expression.parameters)

}

/*!# Schema Object

Every database object which could be created or dropped should
implement the `SchemaObject` trait.
*/
trait SchemaObject {

  def sqlCreate: String

  def sqlDrop: String

  def objectName: String

  override def hashCode = objectName.toLowerCase.hashCode

  override def equals(obj: Any) = obj match {
    case so: SchemaObject => so.objectName.equalsIgnoreCase(this.objectName)
    case _ => false
  }

  override def toString = objectName
}

/*!# Value holders

Value holder is an atomic data-carrier unit of a record. It carries methods for
identifying and manipulating data fields inside persistent records.
*/
trait ValueHolder[T, R <: Record[_, R]]
    extends Container[T]
    with Wrapper[Option[T]]
    with Equals {

  def name: String

  def record: R

  def item = value

  /*!## Equality & Others

  Two fields are considered equal if they belong to the same type of records
  and share the same name.

  The `hashCode` calculation is consistent with `equals` definition.

  The `canEqual` method indicates whether the two fields belong to the same
  type of records.

  Finally, `toString` returns the qualified name of relation which it
  belongs to followed by a dot and the field name.
  */
  def canEqual(that: Any): Boolean = that match {
    case that: ValueHolder[_, _] => this.record.canEqual(that.record)
    case _ => false
  }

  override def equals(that: Any): Boolean = that match {
    case that: ValueHolder[_, _] => this.canEqual(that) &&
        this.name == that.name
    case _ => false
  }

  override lazy val hashCode: Int =  record.relation.qualifiedName.hashCode * 31 +
      name.hashCode

  override def toString: String = record.relation.qualifiedName + "." + name

  /*! The `placeholder` method returns an expression which is used
  to mark a parameter inside JDBC `PreparedStatement` (usually `?` works,
  but custom data-type may require some special treatment).
   */
  def placeholder = ormConf.dialect.placeholder

  /*! ## Composing predicates

  `ValueHolder` provides very basic functionality for predicates composition:

  * `aliasedName` returns the name of this holder qualified with node alias
    (in appropriate context);

  * `EQ` creates an equality predicate (i.e. `column = value` or `column = column`);

  * `NE` creates an inequality predicate (i.e. `column <> value` or
    `column <> column`).

  * `IS_NULL` and `IS_NOT_NULL` creates (not-)nullability predicates
    (i.e. `column IS NULL` or `column IS NOT NULL`).

  More specific predicates can be acquired from subclasses.
  */
  def aliasedName = aliasStack.pop() match {
    case Some(alias: String) => alias + "." + name
    case _ => name
  }

  def EQ(value: T): Predicate =
    new SimpleExpression(ormConf.dialect.EQ(aliasedName, placeholder), List(value))

  def EQ(col: ColumnExpression[_, _]): Predicate =
    new SimpleExpression(ormConf.dialect.EQ(aliasedName, col.toSql), Nil)

  def NE(value: T): Predicate =
    new SimpleExpression(ormConf.dialect.NE(aliasedName, placeholder), List(value))

  def NE(col: ColumnExpression[_, _]): Predicate =
    new SimpleExpression(ormConf.dialect.NE(aliasedName, col.toSql), Nil)

  def IS_NULL: Predicate =
    new SimpleExpression(ormConf.dialect.IS_NULL(aliasedName), Nil)

  def IS_NOT_NULL: Predicate =
    new SimpleExpression(ormConf.dialect.IS_NOT_NULL(aliasedName), Nil)

}

object ValueHolder {

  implicit def toColExpr[T, R <: Record[_, R]]
  (vh: ValueHolder[T, R]): ColumnExpression[T, R] =
    new ColumnExpression(vh)

  implicit def toOrder(vh: ValueHolder[_, _]): Order =
    new Order(vh.aliasedName, Nil)

  implicit def toProjection[T](vh: ValueHolder[T, _]): Projection[T] =
    new ExpressionProjection[T](vh.aliasedName)

}

class ColumnExpression[T, R <: Record[_, R]](column: ValueHolder[T, R])
    extends Expression {

  def parameters = Nil

  val toSql = column.aliasedName

}

/*! `RowResult` is returned from SELECT queries with arbitrary projections.
It contains methods with some heuristics (with tiny overhead)
which help you retrieve Scala-typed results as you would expect
from corresponding projection type.*/
class RowResult {

  val data = new HashMap[String, Any]

  def get[T](name: String) = data.get(name).asInstanceOf[Option[T]]

  def getString(name: String) = data.get(name).map(_.toString)

  def getDouble(name: String) = data.get(name).map(_.toString.toDouble)

  def getInt(name: String) = getDouble(name).map(_.toInt)

  def getLong(name: String) = getDouble(name).map(_.toLong)

  def getBigDecimal(name: String) = data.get(name).map(o => BigDecimal(o.toString))

  def getDate(name: String) = data.get(name).asInstanceOf[Option[Date]]

  def getBoolean(name: String) = data.get(name).asInstanceOf[Option[Boolean]]

}
