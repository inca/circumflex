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

/**
 * Base functionality for SQL sequences.
 */
class Sequence[R](val table: Table[R],
                  val column: Column[Long, R])
    extends SchemaObject with JDBCHelper {

  /**
   * Retrieves next sequence value by invoking backend NEXTVAL query.
   */
  def nextValue: Long = {
    val sql = dialect.selectSequenceNextVal(this)
    sqlLog.debug(sql)
    auto(connectionProvider.getConnection.prepareStatement(sql)) {
      st => auto(st.executeQuery)(rs => if (rs.next) {
        return rs.getLong(1)
      } else
        throw new ORMException("Sequence NEXTVAL did not return a result: " + this.sequenceName))
    }
  }

  /* DDL */

  def sqlCreate = dialect.createSequence(this)

  def sqlDrop = dialect.dropSequence(this)

  def sequenceName = dialect.sequenceName(this)

}