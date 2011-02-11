package ru.circumflex.orm

/*!# Vendor-specific SQL dialects

Following vendors are currently supported by Circumflex ORM:

  * PostgreSQL;
  * MySQL;
  * H2 database;
  * Oracle.
*/

class PostgreSQLDialect extends Dialect {
  override def timestampType = "TIMESTAMPTZ"
}

class MySQLDialect extends Dialect {
  override def driverClass = "com.mysql.jdbc.Driver"

  override def initializeField[R <: Record[_, R]](field: Field[_, R]): Unit = {
    // do nothing -- for MySQL you don't need to create manually a sequence for auto-incrementable fields
  }

  override def defaultExpression[R <: Record[_, R]](field: Field[_, R]): String =
    field match {
      case a: AutoIncrementable[_, _] if (a.autoIncrement_?) =>" AUTO_INCREMENT"
      case _ => field.defaultExpression.map(" DEFAULT " + _).getOrElse("")
    }

  override def identityLastIdPredicate[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): Predicate =
    new SimpleExpression(node.alias + "." + node.relation.PRIMARY_KEY.name + " = LAST_INSERT_ID()", Nil)

  override def identityLastIdQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    new Select(expr[PK]("LAST_INSERT_ID()"))

  override def sequenceNextValQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    throw new UnsupportedOperationException("This operation is unsupported in the MySQL dialect.")
}

class OracleDialect extends Dialect {
  override def driverClass = "oracle.jdbc.driver.OracleDriver"
}

class H2Dialect extends Dialect {
  override def driverClass = "org.h2.Driver"
  override def textType = "VARCHAR"
  override def createIndex(idx: Index): String = {
    var result = "CREATE "
    if (idx.unique_?) result += "UNIQUE "
    result += "INDEX " + idx.name + " ON " + idx.relation.qualifiedName +
        " (" + idx.expression + ")"
    if (idx.where != EmptyPredicate)
      result += " WHERE " + idx.where.toInlineSql
    return result
  }
  override def dropSchema(schema: Schema) = "DROP SCHEMA " + schema.name
}