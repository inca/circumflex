package ru.circumflex.orm

class PostgreSQLDialect extends Dialect

class MySQLDialect extends Dialect {
  override def stringType = "varchar(4096)"
  override def timestampType = "timestamp"
  override def supportsSchema_?() = false
  override def supportDropConstraints_?() = false
  override def qualifyRelation(rel: Relation[_]) = rel.relationName
  override def autoIncrementExpression(col: Column[_, _]) = "auto_increment"
  override def prepareAutoIncrementColumn(col: Column[_, _]) = {}
  override def lastIdExpression(rel: Relation[_]) = "last_insert_id()"
}

class OracleDialect extends Dialect {
  override def stringType = "varchar2(4096)"
  override def timestampType = "timestamp with timezone"
  override def autoIncrementExpression(col: Column[_, _]) = ""
  override def prepareAutoIncrementColumn(col: Column[_, _]): Unit = {
    val seq = new SchemaObject() {
      def objectName = columnSequenceName(col)
      def sqlDrop = "drop sequence " + objectName
      def sqlCreate = "create sequence " + objectName
    }
    val trig = new SchemaObject() {
      def objectName = col.relation.relationName + "_" + col.columnName + "_auto"
      def sqlDrop = "drop trigger " + objectName
      def sqlCreate = "create trigger " + objectName +
              " before insert on " + qualifyRelation(col.relation) + " for each row begin\n" +
              "if :new." + col.columnName + " is null then\n\t" +
              "select " + columnSequenceName(col) + ".nextval into new." + col.columnName +
              " from " + qualifyRelation(col.relation) + ";\n" +
              "end if;\nend;"
    }
    if (!col.relation.preAuxiliaryObjects.contains(seq))
      col.relation.addPreAuxiliaryObjects(seq)
    if (!col.relation.postAuxiliaryObjects.contains(trig))
      col.relation.addPostAuxiliaryObjects(trig)
  }
}




