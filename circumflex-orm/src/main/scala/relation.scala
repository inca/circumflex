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

import collection.mutable.ListBuffer
import ORM._
import java.sql.PreparedStatement
import ru.circumflex.core.Circumflex

/**
 * Designates a relation that can be used to retrieve certain type of records.
 * It can be considered a table, a virtual table, a view, a subquery, etc.
 */
abstract class Relation[R] extends JDBCHelper with QueryHelper {

  protected val _validators = new ListBuffer[RecordValidator[R]]
  protected val _columns = new ListBuffer[Column[_, R]]
  protected val _constraints = new ListBuffer[Constraint[R]]
  protected val _associations = new ListBuffer[Association[R, _]]
  protected val _auxiliaryObjects = new ListBuffer[SchemaObject];

  private var _cachedRecordClass: Class[R] = null;
  private var _cachedRelationName: String = null;

  private var uniqueCounter = -1;

  protected[orm] def uniqueSuffix: String = {
    uniqueCounter += 1
    if (uniqueCounter == 0) return ""
    else return "_" + uniqueCounter
  }

  /**
   * Determines, whether DML operations are allowed on this relation.
   */
  def readOnly: Boolean = false

  /**
   * Returns a class of record which this relation describes.
   */
  def recordClass: Class[R] = {
    if (_cachedRecordClass == null)
      _cachedRecordClass = Class
          .forName(this.getClass.getName.replaceAll("(.*)\\$$", "$1"), true, Circumflex.classLoader)
          .asInstanceOf[Class[R]]
    return _cachedRecordClass
  }

  /**
   * The mandatory primary key constraint for this relation.
   */
  def primaryKey: PrimaryKey[_, R];

  /**
   * Returns Schema object, that will contain specified table.
   * Defaults to DefaultSchema singleton.
   */
  def schema: Schema = DefaultSchema

  /**
   * Provides schema name.
   */
  def schemaName: String = schema.schemaName

  /**
   * Unqualified relation name. Defaults to unqualified record class name
   * (camel-case-to-underscored).
   */
  def relationName: String = {
    if (_cachedRelationName == null)
      _cachedRelationName = recordClass
          .getSimpleName
          .replaceAll("([A-Z])","_$1")
          .replaceAll("^_(.*)","$1")
          .toLowerCase
    return _cachedRelationName
  }


  /**
   * Returns relation's qualified name.
   */
  def qualifiedName: String = dialect.qualifyRelation(this)

  /**
   * Returns validators that correspond to this relation.
   */
  def validators: Seq[RecordValidator[R]] = _validators

  /**
   * Returns columns that correspond to this relation.
   */
  def columns: Seq[Column[_, R]] = _columns

  /**
   * Returns constraints that correspond to this relation.
   */
  def constraints: Seq[Constraint[R]] = _constraints

  /**
   * Returns associations that correspond to this relation.
   */
  def associations: Seq[Association[R, _]] = _associations

  /**
   * Returns sequences associated with this table.
   */
  def sequences: Seq[Sequence[R]] = columns.flatMap(_.sequence)

  /**
   * Returns auxiliary objects associated with this table.
   */
  def auxiliaryObjects: Seq[SchemaObject] = _auxiliaryObjects

  /**
   * If possible, return an association from this relation as parent to
   * specified relation as child.
   */
  def getChildAssociation[C](child: Relation[C]): Option[Association[C, R]] =
    child.getParentAssociation(this)

  /**
   * If possible, return an association from this relation as child to
   * specified relation as parent.
   */
  def getParentAssociation[P](relation: Relation[P]): Option[Association[R, P]] =
    associations.find(_.parentRelation == relation).asInstanceOf[Option[Association[R, P]]]

  /**
   * Returns column list excluding primary key column.
   */
  def nonPKColumns: Seq[Column[_, R]] =
    columns.filter(_ != primaryKey.column)

  /**
   * Returns a node that represents this relation.
   */
  def as(alias: String): RelationNode[R]

  /* SIMPLE QUERIES */

  /**
   * Creates a criteria object for this relation.
   */
  def criteria: Criteria[R] = new Criteria(this)

  /**
   * Queries a record by it's primary key.
   */
  def get(pk: Any): Option[R] =
    criteria.add(_.projection(primaryKey.column) eq pk).unique

  /**
   * Queries all records.
   */
  def all(): Seq[R] =
    criteria.list

  /**
   * Queries specified amount of records.
   */
  def all(limit: Int): Seq[R] =
    criteria.limit(limit).list

  /**
   * Queries specified amount of records, starting from specified offset.
   */
  def all(limit: Int, offset: Int): Seq[R] =
    criteria.limit(limit).offset(offset).list

  /* OBJECT DEFINITIONS */

  /**
   * Creates primary key constraint based on specified column.
   */
  protected[orm] def pk[T](column: Column[T, R]): PrimaryKey[T, R] = {
    val constrName = relationName + "_" + column.columnName + "_pkey";
    val pk = new PrimaryKey(this, constrName , column)
    return pk;
  }

  /**
   * Adds associated auxiliary object.
   */
  protected[orm] def addAuxiliaryObjects(objects: SchemaObject*) = {
    _auxiliaryObjects ++= objects.toList
  }

  /**
   * Adds a unique constraint.
   */
  protected[orm] def unique(columns: Column[_, R]*): UniqueKey[R] = {
    val constrName = relationName + "_" +
        columns.map(_.columnName).mkString("_") + "_key"
    val constr = new UniqueKey(this, constrName, columns.toList)
    _constraints += constr
    return constr
  }

  /**
   * Adds an associative foreign key constraint.
   */
  protected[orm] def foreignKey[T, P](parentRelation: Relation[P],
                                      column: Column[T, R]): AssociativeForeignKey[T, R, P] = {
    val constrName = relationName + "_" + column.columnName + "_fkey"
    val fk = new AssociativeForeignKey(this, parentRelation, constrName, column)
    _constraints += fk
    _associations += fk
    return fk
  }

  /**
   * Adds a foreign key constraint.
   */
  protected[orm] def foreignKey[T, P](parentRelation: Relation[P],
                                      localColumns: Seq[Column[_, R]],
                                      referenceColumns: Seq[Column[_, P]]
      ): ForeignKey[R, P] = {
    val constrName = relationName + "_" + localColumns.map(_.columnName).mkString("_") + "_fkey"
    val fk = new MultiForeignKey(this, parentRelation, constrName, localColumns, referenceColumns)
    _constraints += fk
    return fk
  }

  /**
   * Adds a check constraint with specified name.
   */
  protected[orm] def check(constraintName: String,
                           expression: String): CheckConstraint[R] = {
    val chk = new CheckConstraint(this, constraintName, expression)
    _constraints += chk
    return chk
  }

  /**
   * Adds a check constraint with default name.
   */
  protected[orm] def check(expression: String): CheckConstraint[R] =
    check(relationName + "_check" + uniqueSuffix, expression)

  /**
   * Adds an index with specified name.
   */
  protected[orm] def index(indexName: String): Index[R] = {
    val idx = new Index(this, indexName)
    _auxiliaryObjects += idx
    return idx
  }

  /**
   * Adds an index with default name.
   */
  protected[orm] def index(): Index[R] =
    index(relationName + "_idx" + uniqueSuffix)

  /**
   * Adds one or more columns to relation's definition.
   */
  protected[orm] def addColumns(cols: Column[_, R]*): this.type = {
    _columns ++= cols.toList
    return this
  }

  /**
   * Adds an arbitrary column.
   */
  protected[orm] def column[T](name: String, sqlType: String): Column[T, R] = {
    val col = new Column[T, R](this, name, sqlType)
    addColumns(col)
    return col
  }

  /**
   * Adds a bigint column.
   */
  protected[orm] def longColumn(name: String): LongColumn[R] = {
    val col = new LongColumn(this, name)
    addColumns(col)
    return col
  }

  /**
   * Adds a string column.
   */
  protected[orm] def stringColumn(name: String): StringColumn[R] = {
    val col = new StringColumn(this, name)
    addColumns(col)
    return col
  }

  /**
   * Adds a boolean column.
   */
  protected[orm] def booleanColumn(name: String): BooleanColumn[R] = {
    val col = new BooleanColumn(this, name)
    addColumns(col)
    return col
  }

  /**
   * Adds a timestamp column.
   */
  protected[orm] def timestampColumn(name: String): TimestampColumn[R] = {
    val col = new TimestampColumn(this, name)
    addColumns(col)
    return col
  }

  /* VALIDATION */

  /**
   * Returns None if record has passed validation. Otherwise returns
   * a <code>ValidationError</code> sequence.
   */
  def validate(record: Record[R]): Option[Seq[ValidationError]] = {
    val errors = validators.flatMap(_.apply(record))
    if (errors.size == 0) None
    else Some(errors)
  }

  /**
   * Throws <code>ValidationException</code> if record has failed validation.
   */
  def validate_!(record: Record[R]) = validate(record) match {
    case Some(errors) => throw new ValidationException(errors: _*)
    case _ =>
  }

  /**
   * Adds a field validator.
   */
  protected[orm] def addFieldValidator(col: Column[_, R],
                                       validator: Validator): RecordValidator[R] = {
    val v = new RecordFieldValidator(col, validator)
    _validators += v
    return v
  }

  /* PERSISTENCE STUFF */

  private def setParams(record: Record[R], st: PreparedStatement, cols: Seq[Column[_, R]]) =
    (0 until cols.size).foreach(ix => {
      val col = cols(ix)
      val value = record.getField(col) match {
        case Some(v) => v
        case _ => null
      }
      typeConverter.write(st, value, ix + 1)
    })

  def insert(record: Record[R]): Int = {
    validate_!(record)
    insert_!(record)
  }

  def insert_!(record: Record[R]): Int = {
    if (readOnly)
      throw new ORMException("The relation " + qualifiedName + " is read-only.")
    transactionManager.dml(conn => {
      val sql = dialect.insertRecord(record)
      sqlLog.debug(sql)
      auto(conn.prepareStatement(sql))(st => {
        setParams(record, st, columns)
        return st.executeUpdate
      })
    })
  }

  def update(record: Record[R]): Int = {
    validate_!(record)
    update_!(record)
  }

  def update_!(record: Record[R]): Int = {
    if (readOnly)
      throw new ORMException("The relation " + qualifiedName + " is read-only.")
    transactionManager.dml(conn => {
      val sql = dialect.updateRecord(record)
      sqlLog.debug(sql)
      auto(conn.prepareStatement(sql))(st => {
        setParams(record, st, nonPKColumns)
        typeConverter.write(
          st,
          record.primaryKey.get,
          nonPKColumns.size + 1)
        return st.executeUpdate
      })
    })
  }

  def save(record: Record[R]): Int = {
    validate_!(record)
    save_!(record)
  }

  def save_!(record: Record[R]): Int =
    if (record.identified_?) update_!(record)
    else {
      generateFields(record)
      insert_!(record)
    }

  def delete(record: Record[R]): Int = {
    if (readOnly)
      throw new ORMException("The relation " + qualifiedName + " is read-only.")
    transactionManager.dml(conn => {
      val sql = dialect.deleteRecord(record)
      sqlLog.debug(sql)
      auto(conn.prepareStatement(sql))(st => {
        typeConverter.write(st, record.primaryKey.get, 1)
        return st.executeUpdate
      })
    })
  }

  def generateFields(record: Record[R]): Unit =
    columns.flatMap(_.sequence).foreach(seq => {
      val nextval = seq.nextValue
      record.setField(seq.column, nextval)
    })

  /* EQUALITY AND OTHER STUFF */

  override def toString = qualifiedName

  override def equals(obj: Any) = obj match {
    case rel: Relation[R] => rel.qualifiedName.equalsIgnoreCase(this.qualifiedName)
    case _ => false
  }

  override def hashCode = this.qualifiedName.toLowerCase.hashCode
}