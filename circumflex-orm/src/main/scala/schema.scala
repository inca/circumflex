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

import ORM._
import java.io.Writer
import java.sql.Connection
import org.slf4j.LoggerFactory
import collection.mutable.{HashSet, ListBuffer}

/**
 * Defines a contract for database schema objects.
 * They must provide SQL statement to create them and to drop them.
 */
trait SchemaObject {
  /**
   * SQL statement to create this database object.
   */
  def sqlCreate: String

  /**
   * SQL statement to drop this database object.
   */
  def sqlDrop: String

  /**
   * SQL object name.
   */
  def objectName: String
}

/**
 * Base functionality for SQL schema.
 */
class Schema extends SchemaObject {
  def sqlCreate = dialect.createSchema(this)

  def sqlDrop = dialect.dropSchema(this)

  var schemaName = defaultSchemaName

  def objectName = schemaName

  override def equals(obj: Any) = obj match {
    case sc: Schema => sc.schemaName.equalsIgnoreCase(this.schemaName)
    case _ => false
  }

  override def hashCode = this.schemaName.toLowerCase.hashCode
}

/**
 * Default public schema singleton; used to avoid another abstract method on Table.
 */
object DefaultSchema extends Schema

/**
 * Executes DDL statements.
 */
class DDLExport extends JDBCHelper {
  protected val log = LoggerFactory.getLogger("ru.circumflex.orm")

  val schemata = new HashSet[Schema]()
  val tables = new HashSet[Table[_]]()
  val views = new HashSet[View[_]]()
  val constraints = new HashSet[Constraint[_]]()
  val sequences = new HashSet[Sequence[_]]()
  val auxiliaryObjects = new ListBuffer[SchemaObject]()

  val writers = new HashSet[Writer]()

  def this(tables: Table[_]*) = {
    this()
    tables.toList.foreach(addTable(_))
  }

  def addWriter(writers: Writer *): this.type = {
    this.writers ++= writers.toList
    return this
  }

  def write(msg: String) = {
    writers.foreach(_.write(msg + "\n"))
  }

  /**
   * Adds a table to database objects list.
   */
  def addTable(tab: Table[_]): this.type = {
    tables += tab
    schemata += tab.schema
    constraints ++= tab.constraints
    sequences ++= tab.sequences
    auxiliaryObjects ++= tab.auxiliaryObjects
    return this
  }

  /**
   * Adds a view to database objects list.
   */
  def addView(view: View[_]): this.type = {
    views += view
    schemata += view.schema
    auxiliaryObjects ++= view.auxiliaryObjects
    return this
  }

  def addAuxiliaryObject(obj: SchemaObject): this.type = {
    auxiliaryObjects += obj
    return this
  }

  /**
   * Executes DROP and CREATE script.
   */
  def dropCreate: Unit = {
    drop
    create
  }

  /**
   * Executes CREATE script.
   */
  def create: Unit = {
    // obtain JDBC connection
    autoClose(connectionProvider.getConnection)(conn => {
      // we will commit every successful statement
      conn.setAutoCommit(true)
      // process database objects
      createSchemata(conn)
      createTables(conn)
      createConstraints(conn)
      createSequences(conn)
      createViews(conn)
      createAuxiliaryObjects(conn)
    })(e => log.error("Connection failure occured while exporing DDL.", e))
  }

  /**
   * Executes DROP script.
   */
  def drop: Unit = {
    // obtain JDBC connection
    autoClose(connectionProvider.getConnection)(conn => {
      // we will commit every successful statement
      conn.setAutoCommit(true)
      // process database objects
      dropAuxiliaryObjects(conn)
      dropViews(conn)
      dropSequences(conn)
      dropConstraints(conn)
      dropTables(conn)
      dropSchemata(conn)
    })(e => log.error("Connection failure occured while exporing DDL.", e))
  }

  def dropAuxiliaryObjects(conn: Connection) =
    for (o <- auxiliaryObjects) {
      var msg = ""
      autoClose(conn.prepareStatement(o.sqlDrop))(st => {
        log.debug(o.sqlDrop)
        st.executeUpdate
        msg = "DROP OBJECT " + o.objectName + ": OK"
      })(e => {
        msg = "DROP OBJECT " + o.objectName + ": " + e.getMessage
        log.trace("Error dropping auxiliary object.", e)
      })
      log.info(msg)
      write(msg)
    }

  def dropViews(conn: Connection) =
    for (v <- views) {
      var msg = ""
      autoClose(conn.prepareStatement(v.sqlDrop))(st => {
        log.debug(v.sqlDrop)
        st.executeUpdate
        msg = "DROP VIEW " + v.objectName + ": OK"
      })(e => {
        msg = "DROP VIEW " + v.objectName + ": " + e.getMessage
        log.trace("Error dropping view.", e)
      })
      log.info(msg)
      write(msg)
    }

  def dropSequences(conn: Connection) =
    for (s <- sequences) {
      var msg = ""
      autoClose(conn.prepareStatement(s.sqlDrop))(st => {
        log.debug(s.sqlDrop)
        st.executeUpdate
        msg = "DROP SEQUENCE " + s.objectName + ": OK"
      })(e => {
        msg = "DROP SEQUENCE " + s.objectName + ": " + e.getMessage
        log.trace("Error dropping sequence.", e)
      })
      log.info(msg)
      write(msg)
    }

  def dropConstraints(conn: Connection) =
    for (c <- constraints) {
      var msg = ""
      autoClose(conn.prepareStatement(c.sqlDrop))(st => {
        log.debug(c.sqlDrop)
        st.executeUpdate
        msg = "DROP CONSTRAINT " + c.objectName + ": OK"
      })(e => {
        msg = "DROP CONSTRAINT " + c.objectName + ": " + e.getMessage
        log.trace("Error dropping constraint.", e)
      })
      log.info(msg)
      write(msg)
    }

  def dropTables(conn: Connection) =
    for (t <- tables) {
      var msg = ""
      autoClose(conn.prepareStatement(t.sqlDrop))(st => {
        log.debug(t.sqlDrop)
        st.executeUpdate
        msg = "DROP TABLE " + t.objectName + ": OK"
      })(e => {
        msg = "DROP TABLE " + t.objectName + ": " + e.getMessage
        log.trace("Error dropping table.", e)
      })
      log.info(msg)
      write(msg)
    }

  def dropSchemata(conn: Connection) =
    for (s <- schemata) {
      var msg = ""
      autoClose(conn.prepareStatement(s.sqlDrop))(st => {
        log.debug(s.sqlDrop)
        st.executeUpdate
        msg = "DROP SCHEMA " + s.objectName + ": OK"
      })(e => {
        msg = "DROP SCHEMA " + s.objectName + ": " + e.getMessage
        log.trace("Error dropping schema.", e)
      })
      log.info(msg)
      write(msg)
    }

  def createSchemata(conn: Connection) =
    for (s <- schemata) {
      var msg = ""
      autoClose(conn.prepareStatement(s.sqlCreate))(st => {
        log.debug(s.sqlCreate)
        st.executeUpdate
        msg = "CREATE SCHEMA " + s.objectName + ": OK"
      })(e => {
        msg = "CRAETE SCHEMA " + s.objectName + ": " + e.getMessage
        log.trace("Error creating schema.", e)
      })
      log.info(msg)
      write(msg)
    }

  def createTables(conn: Connection) =
    for (t <- tables) {
      var msg = ""
      autoClose(conn.prepareStatement(t.sqlCreate))(st => {
        log.debug(t.sqlCreate)
        st.executeUpdate
        msg = "CREATE TABLE " + t.objectName + ": OK"
      })(e => {
        msg = "CREATE TABLE " + t.objectName + ": " + e.getMessage
        log.trace("Error creating table.", e)
      })
      log.info(msg)
      write(msg)
    }

  def createConstraints(conn: Connection) =
    for (c <- constraints) {
      var msg = ""
      autoClose(conn.prepareStatement(c.sqlCreate))(st => {
        log.debug(c.sqlCreate)
        st.executeUpdate
        msg = "CREATE CONSTRAINT " + c.objectName + ": OK"
      })(e => {
        msg = "CREATE CONSTRAINT " + c.objectName + ": " + e.getMessage
        log.trace("Error creating constraint.", e)
      })
      log.info(msg)
      write(msg)
    }

  def createSequences(conn: Connection) =
    for (s <- sequences) {
      var msg = ""
      autoClose(conn.prepareStatement(s.sqlCreate))(st => {
        log.debug(s.sqlCreate)
        st.executeUpdate
        msg = "CREATE SEQUENCE " + s.objectName + ": OK"
      })(e => {
        msg = "CREATE SEQUENCE " + s.objectName + ": " + e.getMessage
        log.trace("Error creating sequence.", e)
      })
      log.info(msg)
      write(msg)
    }

  def createViews(conn: Connection) =
    for (v <- views) {
      var msg = ""
      autoClose(conn.prepareStatement(v.sqlCreate))(st => {
        log.debug(v.sqlCreate)
        st.executeUpdate
        msg = "CREATE VIEW " + v.objectName + ": OK"
      })(e => {
        msg = "CREATE VIEW " + v.objectName + ": " + e.getMessage
        log.trace("Error creating view.", e)
      })
      log.info(msg)
      write(msg)
    }

  def createAuxiliaryObjects(conn: Connection) =
    for (o <- auxiliaryObjects) {
      var msg = ""
      autoClose(conn.prepareStatement(o.sqlCreate))(st => {
        log.debug(o.sqlCreate)
        st.executeUpdate
        msg = "CREATE OBJECT " + o.objectName + ": OK"
      })(e => {
        msg = "CREATE OBJECT " + o.objectName + ": " + e.getMessage
        log.trace("Error creating auxiliary object.", e)
      })
      log.info(msg)
      write(msg)
    }

}
