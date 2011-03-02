package ru.circumflex.orm

/*!# Vendor-specific SQL dialects

Following vendors are currently supported by Circumflex ORM:

  * PostgreSQL 8.3+;
  * MySQL;
  * H2 database;
  * Oracle.
*/

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

class PostgreSQLDialect extends Dialect {
  override def driverClass = "org.postgresql.Driver"
  override def timestampType = "TIMESTAMPTZ"

  /*!
  PostgreSQL does not allow this syntax for inserting a row containing only default values:

      INSERT INTO my.table () VALUES ();

  We should write `INSERT INTO my.table DEFAULT VALUES` instead.
  */
  override def insert[PK, R <: Record[PK, R]](dml: Insert[PK, R]): String = {
    var result = "INSERT INTO " + dml.relation.qualifiedName
    if (dml.fields.size > 0)
      result += " (" + dml.fields.map(_.name).mkString(", ") +
        ") VALUES (" + dml.fields.map(_.placeholder).mkString(", ") + ")"
    else result += " DEFAULT VALUES"
    return result
  }
}

class MySQLDialect extends Dialect {
  override def supportsSchema_? = false
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

  override def createIndex(idx: Index): String = {
    var result = "CREATE "
    if (idx.unique_?) result += "UNIQUE "
    result += "INDEX " + idx.name + " USING " + idx.using +
        " ON " + idx.relation.qualifiedName + " (" + idx.expression + ")"
    if (idx.where != EmptyPredicate)
      ORM_LOG.warn("Ignoring WHERE clause of INDEX " + idx.name +
          ": predicates are not supported.")
    return result
  }
}

class OracleDialect extends Dialect {
  override def driverClass = "oracle.jdbc.driver.OracleDriver"

  override def numericType(precision: Int, scale: Int): String =
    "NUMBER" + (if (precision == -1) "" else "(" + precision + "," + scale + ")")
  override def textType = "VARCHAR2(4000)"
  override def varcharType(length: Int): String =
    "VARCHAR2" + (if (length == -1) "" else "(" + length + ")")
  override def booleanType = "NUMBER(1)"
  override def timestampType = "TIMESTAMP"

  override def supportsSchema_? = false

  override def createIndex(idx: Index): String = {
    var result = "CREATE "
    if (idx.unique_?) result += "UNIQUE "
    result += "INDEX " + idx.name +
        " ON " + idx.relation.qualifiedName + " (" + idx.expression + ")"
    if (idx.where != EmptyPredicate)
      ORM_LOG.warn("Ignoring WHERE clause of INDEX " + idx.name +
          ": predicates are not supported.")
    return result
  }


}

class DB2Dialect extends Dialect {
  override def driverClass = "com.ibm.db2.jcc.DB2Driver"
}

class MSSQLDialect extends Dialect {
  override def driverClass = "com.microsoft.sqlserver.jdbc.SQLServerDriver"
  override def booleanType = "BIT"
}