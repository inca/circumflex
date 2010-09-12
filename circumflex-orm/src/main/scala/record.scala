package ru.circumflex.orm

/*!# Record

*/
abstract class Record extends SchemaObject with Equals {

  type PK = Long

  /*!## Record State

  Records in relational theory are distinguished from each other by the value of their
  *primary key*. You should specify what field hold the primary key of your record
  by implementing the `PRIMARY_KEY` method.

  The `transient_?` method indicates, whether the record was not persisted into a database
  yet or it was. The default logic is simple: if the primary key contains `null` then the
  record is *transient* (i.e. not persisted), otherwise the record is considered persistent.
  */
  def PRIMARY_KEY: Field[PK]
  def transient_?(): Boolean = PRIMARY_KEY.null_?

  /*!## Equality & Others
  
  Two record are considered equal if they share the same type and have same primary keys.
  Transient records are never equal to each other.

  The `hashCode` method delegates to record's primary key.

  The `canEqual` method indicates that two records share the same type.

  Finally, the default implementation of `toString` returns fully-qualified class name
  of the record followed by "@" and it's primary key value (or `TRANSIENT` word if
  primary key is `null`).
  */

  override def equals(that: Any) = that match {
    case that: Record => this.PRIMARY_KEY.null_? ^ that.PRIMARY_KEY.null_? &&
      this.PRIMARY_KEY.value == that.PRIMARY_KEY.value &&
      this.getClass == that.getClass
    case _ => false
  }

  override def hashCode = PRIMARY_KEY.hashCode

  def canEqual(that: Any): Boolean = that match {
    case that: Record => this.getClass == that.getClass
    case _ => false
  }

  override def toString = getClass.getSimpleName + "@" +
      PRIMARY_KEY.map(_.toString).getOrElse("TRANSIENT")


}
