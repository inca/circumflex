package ru.circumflex
package orm

import core._

/*!# Record

The record is a central abstraction in Circumflex ORM. Every object persisted into
database should extend the `Record` class. It provides data definition methods
necessary to create a domain model for your application as well as methods for
saving and updating your record to backend database.

Circumflex ORM is all about type safety and domain-specific languages (at a first
glance the record definition may seem a little verbose). Here's the sample definition
of fictional record `Country`:

    class Country extends Record[String, Country] {
      val code = "code".VARCHAR(2)
      val name = "name".TEXT

      def PRIMARY_KEY = code
      val relation = Country
    }

*/
abstract class Record[PK, R <: Record[PK, R]] extends Equals { this: R =>

  implicit def fieldPair2cmp[T1, T2](pair: (Field[T1, R], Field[T2, R])): FieldComposition2[T1, T2, R] =
    composition(pair._1, pair._2)

  implicit def str2ddlHelper(str: String): DefinitionHelper[R] =
    new DefinitionHelper(str, this)

  /*!## Record State

  Records in relational theory are distinguished from each other by the value of their
  _primary key_. You should specify what field hold the primary key of your record
  by implementing the `PRIMARY_KEY` method.

  The `isTransient` method indicates, whether the record was not persisted into a database
  yet or it was. The default logic is simple: if the primary key contains `null` then the
  record is _transient_ (i.e. not persisted), otherwise the record is considered persistent.

  The `relation` method points to the relation from which a record came or to which it
  should go. In general this method should point to the companion object. However, if
  you do not convey to Circumflex ORM conventions, you may specify another object which
  will act a relation for this type of records.
  */
  def PRIMARY_KEY: ValueHolder[PK, R]
  def isTransient: Boolean = PRIMARY_KEY.isEmpty
  def relation: Relation[PK, R]
  def uuid = getClass.getName

  /*!## Persistence & Validation

  The `refresh` method is used to synchronize an already persisted record with its state in backend.
  It evicts the record from cache and performs SQL `SELECT` using primary key-based predicate.

  The `INSERT_!`, `UPDATE_!` and `DELETE_!` methods are used to insert, update or delete a single
  record. The `INSERT` and `UPDATE` do the same as their equivalents except that validation
  is performed before actual execution. The `refresh` method performs select with primary key
  criteria and updates the fields with retrieved values.

  When inserting new record into database the primary key should be generated. It is done either
  by polling database, by supplying `NULL` in primary key and then querying last generated identifier
  or manually by application. The default implementation relies on application-assigned identifiers;
  to use different strategy mix in one of the `Generator` traits or simply override the `persist`
  method.
  */
  def refresh(): this.type = if (isTransient)
    throw new ORMException("Could not refresh transient record.")
  else {
    val root = relation.AS("root")
    val id = PRIMARY_KEY()
    tx.cache.evictRecord(id, relation)
    SELECT(root.*).FROM(root).WHERE(root.PRIMARY_KEY EQ id).unique() match {
      case Some(r: R) =>
        relation.copyFields(r, this)
        this
      case _ =>
        throw new ORMException("Could not refresh a record because it is missing in the backend.")
    }
  }

  def INSERT_!(fields: Field[_, R]*): Int = if (relation.isReadOnly)
    throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")
  else {
    // Execute events
    relation.beforeInsert.foreach(c => c(this))
    // Prepare and execute query
    val result = _persist(evalFields(fields))
    // Update cache
    tx.cache.evictRecord(PRIMARY_KEY(), relation)
    tx.cache.cacheRecord(PRIMARY_KEY(), relation, Some(this))
    // Execute events
    relation.afterInsert.foreach(c => c(this))
    result
  }

  def INSERT(fields: Field[_, R]*): Int = {
    validate_!()
    INSERT_!(fields: _*)
  }

  protected def _persist(fields: Seq[Field[_, R]]): Int = PRIMARY_KEY.value match {
    case Some(id: PK) =>
      val result = new Insert(relation, fields.filter(!_.isEmpty)).execute()
      if (relation.isAutoRefresh) refresh()
      result
    case _ => throw new ORMException("Application-assigned identifier is expected. " +
        "Use one of the generators if you wish identifiers to be generated automatically.")
  }

  def UPDATE_!(fields: Field[_, R]*): Int = if (relation.isReadOnly)
    throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")
  else {
    if (PRIMARY_KEY.isEmpty)
      throw new ORMException("Update is only allowed with non-null PRIMARY KEY field.")
    // Execute events
    relation.beforeUpdate.foreach(c => c(this))
    // Collect fields which will participate in query
    val f = evalFields(fields).filter(_ != PRIMARY_KEY)
    // Prepare and execute a query
    val q = (relation AS "root")
        .map(r => r.criteria.add(r.PRIMARY_KEY EQ PRIMARY_KEY())).mkUpdate()
    f.foreach(f => q.SET[Any](f.asInstanceOf[Field[Any, R]], f.value))
    val result = q.execute()
    if (relation.isAutoRefresh) refresh()
    // Invalidate caches
    tx.cache.evictInverse[PK, R](this)
    tx.cache.evictRecord(PRIMARY_KEY(), relation)
    tx.cache.cacheRecord(PRIMARY_KEY(), relation, Some(this))
    // Execute events
    relation.afterUpdate.foreach(c => c(this))
    result
  }

  def UPDATE(fields: Field[_, R]*): Int = {
    validate_!()
    UPDATE_!(fields: _*)
  }

  def DELETE_!(): Int = if (relation.isReadOnly)
    throw new ORMException("The relation " + relation.qualifiedName + " is read-only.")
  else {
    if (PRIMARY_KEY.isEmpty)
      throw new ORMException("Delete is only allowed with non-null PRIMARY KEY field.")
    // Execute events
    relation.beforeDelete.foreach(c => c(this))
    // Prepare and execute query
    val result = (relation AS "root")
        .map(r => r.criteria.add(r.PRIMARY_KEY EQ PRIMARY_KEY())).mkDelete().execute()
    // Invalidate caches
    tx.cache.evictRecord(PRIMARY_KEY(), relation)
    tx.cache.evictInverse[PK, R](this)
    // Execute events
    relation.afterDelete.foreach(c => c(this))
    result
  }

  def validate(): Option[Seq[Msg]] = {
    val errors = relation.validation.validate(this)
    if (errors.size <= 0) None
    else Some(List(errors: _*))
  }

  def validate_!() = validate().map(errors => throw new ValidationException(errors))

  def save_!(): Int = if (isTransient)
    throw new ORMException("Application-assigned identifier is expected. " +
        "Use one of the generators if you wish identifiers to be generated automatically.")
  else relation.get(PRIMARY_KEY()) match {
    case Some(_) => UPDATE_!()
    case _ => INSERT_!()
  }

  def save(): Int = {
    validate_!()
    save_!()
  }

  // Internal helpers

  protected def evalFields(fields: Seq[Field[_, R]]): Seq[Field[_, R]] =
    (if (fields.size == 0) relation.fields else fields)
        .map(f => relation.getField(this, f))

  /*!## Field Compositions
  
  Fields can be grouped into field compositions using the `composition` method.
  Compositions can be used as primary keys and participate in simple queries.
  Circumflex ORM currently supports only composition with arity of 2. A pair
  of fields is implicitly converted into `FieldComposition2` when necessary.
  */
  def composition[T1, T2](f1: Field[T1, R], f2: Field[T2, R]) =
    new FieldComposition2[T1, T2, R](f1, f2, this)

  /*!## Inverse Associations

  One-to-one and one-to-many relationships can be implemented using `inverseOne`
  or `inverseMany` methods.
  */
  def inverseOne[C <: Record[_, C]](association: Association[PK, C, R]) =
    new InverseOne[PK, C, R](this, association)

  def inverseMany[C <: Record[_, C]](association: Association[PK, C, R]) =
    new InverseMany[PK, C, R](this, association)

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
    case that: Record[_, _] =>
      !(this.PRIMARY_KEY.isEmpty || that.PRIMARY_KEY.isEmpty) &&
          this.PRIMARY_KEY.value == that.PRIMARY_KEY.value &&
          this.getClass == that.getClass
    case _ => false
  }

  override def hashCode = PRIMARY_KEY.hashCode

  override def canEqual(that: Any): Boolean = that match {
    case that: Relation[_, _] =>
      this.getClass == that.recordClass
    case that: Record[_, _] =>
      this.getClass == that.getClass
    case _ => false
  }

  override def toString = getClass.getSimpleName + "@" +
      PRIMARY_KEY.map(_.toString).getOrElse("TRANSIENT")

}

/*!# Identity Generation Strategies

Different identity generation strategies can be used by mixing in one of the `Generator`
traits. Following identity generators are supported out-of-box:

  * application-assigned identifiers (the default one, no need to mixin traits): application
  is responsible for generating and assigning identifiers before attempting to persist a record;
  * `IdentityGenerator` is a database-specific strategy: application should persist a record
  with `NULL` primary key value, database is responsible for generating an identifier value and
  for exposing last generated identifier;
  * `SequenceGenerator` assumes that database supports sequences: the database is polled for
  next sequence value which is then used as an identifier for persisting.
*/
trait Generator[PK, R <: Record[PK, R]] extends Record[PK, R] { this: R =>
  override protected def _persist(fields: scala.Seq[Field[_, R]]): Int = persist(fields)
  def persist(fields: Seq[Field[_, R]]): Int
  override def save_!(): Int = if (isTransient) INSERT_!() else UPDATE_!()
}

trait IdentityGenerator[PK, R <: Record[PK, R]] extends Generator[PK, R] { this: R =>
  def persist(fields: scala.Seq[Field[_, R]]): Int = {
    // Make sure that PRIMARY_KEY contains `NULL`
    this.PRIMARY_KEY.setNull()
    // Persist all not-null fields
    val result = new Insert(relation, fields.filter(!_.isEmpty)).execute()
    // Fetch either the whole record or just an identifier.
    val root = relation.AS("root")
    if (relation.isAutoRefresh)
      SELECT(root.*).FROM(root).WHERE(ormConf.dialect.identityLastIdPredicate(root)).unique() match {
        case Some(r: R) => relation.copyFields(r, this)
        case _ => throw new ORMException("Backend didn't return last inserted record. " +
            "Try another identifier generation strategy.")
      }
    else ormConf.dialect.identityLastIdQuery(root).unique() match {
      case Some(id: PK) => this.PRIMARY_KEY := id
      case _ => throw new ORMException("Backend didn't return last generated identity. " +
          "Try another identifier generation strategy.")
    }
    result
  }
}

trait SequenceGenerator[PK, R <: Record[PK, R]] extends Generator[PK, R] { this: R =>
  def persist(fields: scala.Seq[Field[_, R]]): Int = {
    // Poll database for next sequence value
    val root = relation.AS("root")
    ormConf.dialect.sequenceNextValQuery(root).unique() match {
      case Some(id: PK) =>
        // Assign retrieved id and persist all not-null fields
        val f = fields.map { f =>
          if (f == this.PRIMARY_KEY)
            f.asInstanceOf[Field[PK, R]].set(Some(id))
          f
        }.filter(!_.isEmpty)
        val result = new Insert(relation, f).execute()
        // Perform additional select if required
        if (relation.isAutoRefresh)
          refresh()
        result
      case _ => throw new ORMException("Backend didn't return next sequence value. " +
          "Try another identifier generation strategy.")
    }
  }
}
