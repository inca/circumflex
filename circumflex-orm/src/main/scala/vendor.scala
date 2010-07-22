package ru.circumflex.orm

class PostgreSQLDialect extends Dialect

class MySQLDialect extends Dialect {
  override def textType = "VARCHAR(4096)"
  override def timestampType = "TIMESTAMP"
  override def supportsSchema_?(): Boolean = false
  override def supportsDropConstraints_?(): Boolean = false
  override def relationQualifiedName(relation: Relation[_]) = quoteTable(relation.relationName)
  override def primaryKeyExpression(record: Record[_]) = "AUTO_INCREMENT"
  override def initializeRelation(relation: Relation[_]) = {}
  override def lastIdExpression(node: RelationNode[_]) =
    node.alias + "." + node.relation.primaryKey.name + " = LAST_INSERT_ID()"

  /**
   * Quote identifier with MySQL backtick.
   */
  override def quoteIdentifer(identifier: String) = "`" + identifier + "`"

  /**
   * Quote table with MySQL backtick.
   */
  override def quoteTable(table: String) = quoteIdentifer(table).replace(".", "`.`")
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
