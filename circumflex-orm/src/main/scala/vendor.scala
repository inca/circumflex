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

class MySQLDialect extends Dialect

class OracleDialect extends Dialect

class H2Dialect extends Dialect {
  override def textType = "VARCHAR"
  override def createIndex(idx: Index): String = {
    var result = "CREATE "
    if (idx.unique_?) result += "UNIQUE "
    result += "INDEX " + quoteIdentifier(idx.name) + " ON " + idx.relation.qualifiedName +
        " (" + idx.expression + ")"
    if (idx.where != EmptyPredicate)
      result += " WHERE " + idx.where.toInlineSql
    return result
  }
  override def dropSchema(schema: Schema) = "DROP SCHEMA " + schema.name
}