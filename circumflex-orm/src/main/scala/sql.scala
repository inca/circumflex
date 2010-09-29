package ru.circumflex.orm

/*!# Schema objects

Following classes represent various database schema objects:

  * `Schema` corresponds to database schema (or catalog), if such objects
  are supported by database vendor;
  * `Constraint` corresponds to one of database constraint types:

    * `Unique`,
    * `ForeignKey`,
    * `CheckConstraint`;

  * `Index` corresponds to database index.

Circumflex ORM also uses some helpers to make DSL-style data definition.
*/

class Schema(val name: String) extends SchemaObject {
  def objectName = "SCHEMA " + name
  def sqlCreate = dialect.createSchema(this)
  def sqlDrop = dialect.dropSchema(this)
}

abstract class Constraint(val constraintName: String,
                          val relation: Relation[_, _])
    extends SchemaObject with SQLable {

  def objectName = "CONSTRAINT " + constraintName
  def sqlCreate = dialect.alterTableAddConstraint(this)
  def sqlDrop = dialect.alterTableDropConstraint(this)
  def toSql = dialect.constraintDefinition(this)

  def sqlDefinition: String

  override def toString = toSql
}

class UniqueKey(name: String,
                relation: Relation[_, _],
                val fields: Seq[Field[_, _]])
    extends Constraint(name, relation) {
  def sqlDefinition = dialect.uniqueKeyDefinition(this)
}

class ForeignKey(name: String,
                 childRelation: Relation[_, _],
                 val childFields: Seq[Field[_, _]],
                 val parentRelation: Relation[_, _],
                 val parentFields: Seq[Field[_, _]])
    extends Constraint(name, childRelation) {

  protected var _onDelete: ForeignKeyAction = NO_ACTION
  def onDelete = _onDelete
  def ON_DELETE(action: ForeignKeyAction): this.type = {
    _onDelete = action
    return this
  }

  protected var _onUpdate: ForeignKeyAction = NO_ACTION
  def onUpdate = _onUpdate
  def ON_UPDATE(action: ForeignKeyAction): this.type = {
    _onUpdate = action
    return this
  }

  def sqlDefinition = dialect.foreignKeyDefinition(this)
}

class CheckConstraint(name: String,
                      relation: Relation[_, _],
                      val expression: String)
    extends Constraint(name, relation) {
  def sqlDefinition = dialect.checkConstraintDefinition(this)
}

class Index(val name: String,
            val relation: Relation[_, _],
            val expression: String)
    extends SchemaObject {

  protected var _unique: Boolean = false
  def unique_?() = _unique
  def UNIQUE: this.type = {
    this._unique = true
    return this
  }

  private var _method: String = "btree"
  def using = _method
  def USING(method: String): this.type = {
    this._method = method
    return this
  }

  private var _where: Predicate = EmptyPredicate
  def where = _where
  def WHERE(predicate: Predicate): this.type = {
    this._where = predicate
    return this
  }

  def objectName = "INDEX " + name
  def sqlCreate = dialect.createIndex(this)
  def sqlDrop = dialect.dropIndex(this)
}

class ConstraintHelper(name: String, relation: Relation[_, _]) {
  def UNIQUE(fields: Field[_, _]*): UniqueKey =
    new UniqueKey(name, relation, fields.toList)

  def CHECK(expression: String): CheckConstraint =
    new CheckConstraint(name, relation, expression)

  def FOREIGN_KEY(parentRelation: Relation[_, _],
                  childFields: Seq[Field[_, _]],
                  parentFields: Seq[Field[_, _]]): ForeignKey =
    new ForeignKey(name, relation, childFields, parentRelation, parentFields)

  def FOREIGN_KEY(parentRelation: Relation[_, _],
                  fields: (Field[_, _], Field[_, _])*): ForeignKey = {
    val childFields = fields.map(_._1)
    val parentFields = fields.map(_._2)
    return FOREIGN_KEY(parentRelation, childFields, parentFields)
  }

  def FOREIGN_KEY(localFields: Field[_, _]*): ForeignKeyHelper =
    new ForeignKeyHelper(name, relation, localFields)
}

class ForeignKeyHelper(name: String, childRelation: Relation[_, _], childFields: Seq[Field[_, _]]) {
  def REFERENCES(parentRelation: Relation[_, _],
                 parentFields: Field[_, _]*): ForeignKey =
    new ForeignKey(name, childRelation, childFields, parentRelation, parentFields)
}

class DefinitionHelper[R <: Record[_, R]](name: String, record: R) {
  def INTEGER = new IntField(name, record)
  def BIGINT = new LongField(name, record)
  def NUMERIC(precision: Int = -1, scale: Int = 0) = new NumericField(name, record, precision, scale)
  def TEXT = new TextField(name, record, dialect.textType)
  def VARCHAR(length: Int = -1) = new TextField(name, record, length)
  def BOOLEAN = new BooleanField(name, record)
  def DATE = new DateField(name, record)
  def TIME = new TimeField(name, record)
  def TIMESTAMP = new TimestampField(name, record)
  def XML = new XmlField(name, record)

  def INDEX(expression: String) = new Index(name, record.relation, expression)
}

case class ForeignKeyAction(val toSql: String) extends SQLable {
  override def toString = toSql
}

case class JoinType(val toSql: String) extends SQLable {
  override def toString = toSql
}

case class SetOperation(val toSql: String) extends SQLable {
  override def toString = toSql
}

class Order(val expression: String, val parameters: Seq[Any])
    extends ParameterizedExpression {
  protected[orm] var _specificator = dialect.asc
  def ASC: this.type = {
    this._specificator = dialect.asc
    return this
  }
  def DESC: this.type = {
    this._specificator = dialect.desc
    return this
  }
  def toSql = expression + " " + _specificator
}