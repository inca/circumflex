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




