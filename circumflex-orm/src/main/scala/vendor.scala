package ru.circumflex
package orm

import java.sql._

/*!# Vendor-specific SQL dialects

Following vendors are currently supported by Circumflex ORM:

  * PostgreSQL 8.3+;
  * MySQL 5.7+;
  * H2 database.

We also provide limited support for `Oracle`, `MS SQL Server` and `DB2`.
We would appreciate any commits for better vendors support.
*/
class H2Dialect extends Dialect {
  override def driverClass = "org.h2.Driver"
  override def textType = "VARCHAR"
  override def createIndex(idx: Index): String = {
    var result = "CREATE "
    if (idx.isUnique) result += "UNIQUE "
    result += "INDEX " + idx.name + " ON " + idx.relation.qualifiedName +
        " (" + idx.expression + ")"
    if (idx.whereClause != EmptyPredicate)
      result += " WHERE " + idx.whereClause.toInlineSql
    result
  }
  override def dropSchema(schema: Schema) = "DROP SCHEMA " + schema.name
}

class PostgreSQLDialect extends Dialect {
  override def driverClass = "org.postgresql.Driver"
  override def timestampType = "TIMESTAMPTZ"
}

class MySQLDialect extends Dialect {
  override def supportsSchema = false
  override def driverClass = "com.mysql.jdbc.Driver"

  override def initializeField[R <: Record[_, R]](field: Field[_, R]) {
    // do nothing -- for MySQL you don't need to create manually a sequence for auto-incrementable fields
  }

  override def defaultExpression[R <: Record[_, R]](field: Field[_, R]): String =
    field match {
      case a: AutoIncrementable[_, _] if (a.isAutoIncrement) =>" AUTO_INCREMENT"
      case _ => field.defaultExpression.map(" DEFAULT " + _).getOrElse("")
    }

  override def identityLastIdPredicate[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): Predicate =
    new SimpleExpression(node.alias + "." + node.relation.PRIMARY_KEY.name + " = LAST_INSERT_ID()", Nil)

  override def identityLastIdQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    new Select(expr[PK]("LAST_INSERT_ID()"))

  override def sequenceNextValQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    throw new UnsupportedOperationException("This operation is unsupported in MySQL dialect.")

  override def createIndex(idx: Index): String = {
    var result = "CREATE "
    if (idx.isUnique) result += "UNIQUE "
    result += "INDEX " + idx.name + " USING " + idx.usingClause +
        " ON " + idx.relation.qualifiedName + " (" + idx.expression + ")"
    if (idx.whereClause != EmptyPredicate)
      ORM_LOG.warn("Ignoring WHERE clause of INDEX " + idx.name +
          ": predicates are not supported.")
    result
  }

  override def delete[PK, R <: Record[PK, R]](dml: Delete[PK, R]): String = {
    var result = "DELETE " + dml.node.alias + " FROM " + dml.node.toSql
    if (dml.whereClause != EmptyPredicate) result += " WHERE " + dml.whereClause.toSql
    result
  }
}

class OracleDialect extends Dialect {
  override def driverClass = "oracle.jdbc.driver.OracleDriver"

  override def fkNoAction = "SET_NULL"
  override def fkRestrict = "SET NULL"
  override def fkSetDefault = "SET NULL"

  override def numericType(precision: Int, scale: Int): String =
    "NUMBER" + (if (precision == -1) "" else "(" + precision + "," + scale + ")")
  override def textType = "VARCHAR2(4000)"
  override def varcharType(length: Int): String =
    "VARCHAR2" + (if (length == -1) "" else "(" + length + ")")
  override def booleanType = "NUMBER(1)"

  override def supportsSchema = false

  override def createIndex(idx: Index): String = {
    var result = "CREATE "
    if (idx.isUnique) result += "UNIQUE "
    result += "INDEX " + idx.name +
        " ON " + idx.relation.qualifiedName + " (" + idx.expression + ")"
    if (idx.whereClause != EmptyPredicate)
      ORM_LOG.warn("Ignoring WHERE clause of INDEX " + idx.name +
          ": predicates are not supported.")
    result
  }

  override def defaultExpression[R <: Record[_, R]](field: Field[_, R]): String =
    field.defaultExpression.map(" DEFAULT " + _).getOrElse("")

  override def sequenceNextValQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    new Select(expr[PK](sequenceName(node.relation.PRIMARY_KEY) + ".nextval FROM dual"))
  override def identityLastIdPredicate[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): Predicate =
    throw new UnsupportedOperationException("This operation is unsupported in Oracle dialect.")
  override def identityLastIdQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    throw new UnsupportedOperationException("This operation is unsupported in Oracle dialect.")

}

class DB2Dialect extends Dialect {
  override def driverClass = "com.ibm.db2.jcc.DB2Driver"

  override def prepareStatement(conn: Connection, sql: String): PreparedStatement =
    conn.prepareStatement(sql, ResultSet.TYPE_SCROLL_INSENSITIVE)

  override def textType = "VARCHAR(4000)"
  override def booleanType = "SMALLINT"

  override def defaultExpression[R <: Record[_, R]](field: Field[_, R]): String =
    field match {
      case a: AutoIncrementable[_, _] if (a.isAutoIncrement) => " GENERATED BY DEFAULT AS IDENTITY"
      case _ => field.defaultExpression.map(" DEFAULT " + _).getOrElse("")
    }

  override def sequenceNextValQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    new NativeSQLQuery[PK](expr[PK]("NEXTVAL FOR " + sequenceName(node.relation.PRIMARY_KEY)),
      prepareExpr("SELECT {*} FROM SYSIBM.SYSDUMMY1"))
  override def identityLastIdPredicate[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): Predicate =
    new SimpleExpression(node.alias + "." + node.relation.PRIMARY_KEY.name + " = IDENTITY_VAL_LOCAL()", Nil)
  override def identityLastIdQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    new NativeSQLQuery[PK](expr[PK]("IDENTITY_VAL_LOCAL()"),
      prepareExpr("SELECT {*}  FROM SYSIBM.SYSDUMMY1"))

  override def createIndex(idx: Index): String = {
    var result = "CREATE "
    if (idx.isUnique) result += "UNIQUE "
    result += "INDEX " + idx.name +
        " ON " + idx.relation.qualifiedName + " (" + idx.expression + ")"
    if (idx.whereClause != EmptyPredicate)
      ORM_LOG.warn("Ignoring WHERE clause of INDEX " + idx.name +
          ": predicates are not supported.")
    result
  }

  override def dropSchema(schema: Schema) = "DROP SCHEMA " + schema.name + " RESTRICT"

  override def insert[PK, R <: Record[PK, R]](dml: Insert[PK, R]): String = {
    var result = "INSERT INTO " + dml.relation.qualifiedName
    if (dml.fields.size > 0)
      result += " (" + dml.fields.map(_.name).mkString(", ") +
          ") VALUES (" + dml.fields.map(_.placeholder).mkString(", ") + ")"
    else result += " VALUES DEFAULT"
    result
  }

}

class MSSQLDialect extends Dialect {
  override def driverClass = "com.microsoft.sqlserver.jdbc.SQLServerDriver"

  override def booleanType = "BIT"

  override def supportsSchema = false

  override def defaultExpression[R <: Record[_, R]](field: Field[_, R]): String =
    field match {
      case a: AutoIncrementable[_, _] if (a.isAutoIncrement) =>" IDENTITY"
      case _ => field.defaultExpression.map(" DEFAULT " + _).getOrElse("")
    }

  override def columnDefinition[R <: Record[_, R]](field: Field[_, R]): String = {
    var result = field.name + " " + field.sqlType
    result += defaultExpression(field)
    if (field.isNotNull) result += " NOT NULL"
    result
  }

  override def createIndex(idx: Index): String = {
    var result = "CREATE "
    if (idx.isUnique) result += "UNIQUE "
    result += "INDEX " + idx.name + " ON " + idx.relation.qualifiedName +
        " (" + idx.expression + ")"
    if (idx.whereClause != EmptyPredicate)
      result += " WHERE " + idx.whereClause.toInlineSql
    result
  }

  override def sequenceNextValQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    throw new UnsupportedOperationException("This operation is unsupported in MS SQL Server dialect.")

  override def identityLastIdPredicate[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): Predicate =
    new SimpleExpression(node.alias + "." + node.relation.PRIMARY_KEY.name + " = @@IDENTITY", Nil)

  override def identityLastIdQuery[PK, R <: Record[PK, R]](node: RelationNode[PK, R]): SQLQuery[PK] =
    new Select(expr[PK]("@@IDENTITY"))

}