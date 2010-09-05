package ru.circumflex.orm

class H2Dialect extends Dialect {
  override def textType = "varchar"
  override def createIndex(idx: Index): String = {
    var result = "CREATE "
    if (idx.unique_?) result += "UNIQUE "
    result += "INDEX " + quoteIdentifer(idx.name) + " ON " + idx.relation.qualifiedName +
        " (" + idx.expression + ")"
    if (idx.where != EmptyPredicate)
      result += " WHERE " + idx.where.toInlineSql
    return result
  }
  override def dropSchema(schema: Schema) = "DROP SCHEMA " + schema.name
}

class PostgreSQLDialect extends Dialect {
  override def quoteIdentifer(identifier: String) = "\"" + identifier + "\""
}

class MySQLDialect extends Dialect {
  override def textType = "VARCHAR(4096)"
  override def timestampType = "TIMESTAMP"
  override def supportsSchema_?(): Boolean = false
  override def supportsDropConstraints_?(): Boolean = false
  override def primaryKeyExpression(record: Record[_]) = "AUTO_INCREMENT"
  override def initializeRelation(relation: Relation[_]) = {}
  override def lastIdExpression(node: RelationNode[_]) =
    node.alias + "." + node.relation.primaryKey.name + " = LAST_INSERT_ID()"
  override def quoteIdentifer(identifier: String) = "`" + identifier + "`"
}

class OracleDialect extends Dialect {
  override def textType = "VARCHAR2(4096)"
  override def timestampType = "TIMESTAMP WITH TIMEZONE"
  override def primaryKeyExpression(record: Record[_]) = ""
  override def initializeRelation(relation: Relation[_]) = {
    val seqName = pkSequenceName(relation)
    val seq = new SchemaObject {
      val objectName = "SEQUENCE " + seqName
      val sqlDrop = "DROP SEQUENCE " + seqName
      val sqlCreate = "CREATE SEQUENCE " + seqName
    }
    val trigName = relation.relationName + "_id_auto"
    val trig = new SchemaObject() {
      def objectName = "TRIGGER " + trigName
      def sqlDrop = "DROP TRIGGER " + trigName
      def sqlCreate = "CREATE TRIGGER " + trigName +
              " BEFORE INSERT ON " + relation.qualifiedName + " FOR EACH ROW BEGIN\n" +
              "IF :new." + relation.primaryKey.name + " IS NULL THEN\n\t" +
              "SELECT " + seqName + ".NEXTVAL INTO NEW." + relation.primaryKey.name +
              " FROM " + relation.qualifiedName + ";\n" +
              "END IF;\nEND;"
    }
    relation.addPreAux(seq)
    relation.addPostAux(trig)
  }
}
