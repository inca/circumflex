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

  override def toString = objectName
}

/**
 * Base functionality for SQL schema.
 */
class Schema(var schemaName: String) extends SchemaObject {

  def sqlCreate = dialect.createSchema(this)
  def sqlDrop = dialect.dropSchema(this)

  def objectName = schemaName
  
  override def equals(obj: Any) = obj match {
    case sc: Schema => sc.schemaName.equalsIgnoreCase(this.schemaName)
    case _ => false
  }

  override def hashCode = this.schemaName.toLowerCase.hashCode
}

/**
 * Default public schema singleton.
 */
object DefaultSchema extends Schema(defaultSchemaName)

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

  val infoMsgs = new ListBuffer[String]()
  val errMsgs = new ListBuffer[String]()

  def this(objList: SchemaObject*) = {
    this()
    objList.toList.foreach(addObject(_))
  }

  def addWriter(writers: Writer *): this.type = {
    this.writers ++= writers.toList
    return this
  }

  def write(msg: String) = {
    writers.foreach(_.write(msg + "\n"))
  }

  def addObject(obj: SchemaObject): this.type = {
    obj match {
      case t: Table[_] => {
        tables += t
        addObject(t.schema)
        t.constraints.foreach(o => addObject(o))
        t.sequences.foreach(o => addObject(o))
        t.auxiliaryObjects.foreach(o => addObject(o))
      }
      case v: View[_] => {
        views += v
        schemata += v.schema
        v.auxiliaryObjects.foreach(o => addObject(o))
      }
      case s: Sequence[_] => sequences += s
      case c: Constraint[_] => constraints += c
      case s: Schema => schemata += s
      case o => auxiliaryObjects += o
    }
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
  def create: Unit =
    autoClose(connectionProvider.getConnection)(conn => {
      // we will commit every successful statement
      val autoCommit = conn.getAutoCommit
      conn.setAutoCommit(true)
      // clear statistics
      infoMsgs.clear
      errMsgs.clear
      // process database objects
      log.info("Executing schema create script.")
      createSchemata(conn)
      createTables(conn)
      createConstraints(conn)
      createSequences(conn)
      createViews(conn)
      createAuxiliaryObjects(conn)
      // restore previous auto-commit setting
      conn.setAutoCommit(autoCommit)
      // report log and statistics
      infoMsgs.foreach(log.info(_))
      errMsgs.foreach(log.info(_))
      log.info("Create schema script finished.")
      log.info("{} statements executes successfully.", infoMsgs.size)
      log.info("{} statements failed.", errMsgs.size)
      infoMsgs.foreach(write(_))
      errMsgs.foreach(write(_))
    })(e => log.error("Connection failure occured while exporing DDL.", e))

  /**
   * Executes DROP script.
   */
  def drop: Unit =
    autoClose(connectionProvider.getConnection)(conn => {
      // we will commit every successful statement
      val autoCommit = conn.getAutoCommit
      conn.setAutoCommit(true)
      // clear statistics
      infoMsgs.clear
      errMsgs.clear
      // process database objects
      log.info("Executing schema drop script.")
      dropAuxiliaryObjects(conn)
      dropViews(conn)
      dropSequences(conn)
      dropConstraints(conn)
      dropTables(conn)
      dropSchemata(conn)
      // restore previous auto-commit setting
      conn.setAutoCommit(autoCommit)
      // report log and statistics
      infoMsgs.foreach(log.info(_))
      errMsgs.foreach(log.info(_))
      log.info("Drop schema script finished.")
      log.info("{} statements executes successfully.", infoMsgs.size)
      log.info("{} statements failed.", errMsgs.size)
      infoMsgs.foreach(write(_))
      errMsgs.foreach(write(_))
    })(e => log.error("Connection failure occured while exporing DDL.", e))

  def dropAuxiliaryObjects(conn: Connection) =
    for (o <- auxiliaryObjects.reverse)
      autoClose(conn.prepareStatement(o.sqlDrop))(st => {
        log.debug(o.sqlDrop)
        st.executeUpdate
        infoMsgs += ("DROP OBJECT " + o.objectName + ": OK")
      })(e => {
        errMsgs += ("DROP OBJECT " + o.objectName + ": " + e.getMessage)
        log.trace("Error dropping auxiliary object.", e)
      })

  def dropViews(conn: Connection) =
    for (v <- views) {
      autoClose(conn.prepareStatement(v.sqlDrop))(st => {
        log.debug(v.sqlDrop)
        st.executeUpdate
        infoMsgs += ("DROP VIEW " + v.objectName + ": OK")
      })(e => {
        errMsgs += ("DROP VIEW " + v.objectName + ": " + e.getMessage)
        log.trace("Error dropping view.", e)
      })
    }

  def dropSequences(conn: Connection) =
    for (s <- sequences)
      autoClose(conn.prepareStatement(s.sqlDrop))(st => {
        log.debug(s.sqlDrop)
        st.executeUpdate
        infoMsgs += ("DROP SEQUENCE " + s.objectName + ": OK")
      })(e => {
        errMsgs += ("DROP SEQUENCE " + s.objectName + ": " + e.getMessage)
        log.trace("Error dropping sequence.", e)
      })

  def dropConstraints(conn: Connection) =
    for (c <- constraints)
      autoClose(conn.prepareStatement(c.sqlDrop))(st => {
        log.debug(c.sqlDrop)
        st.executeUpdate
        infoMsgs += ("DROP CONSTRAINT " + c.objectName + ": OK")
      })(e => {
        errMsgs += ("DROP CONSTRAINT " + c.objectName + ": " + e.getMessage)
        log.trace("Error dropping constraint.", e)
      })

  def dropTables(conn: Connection) =
    for (t <- tables)
      autoClose(conn.prepareStatement(t.sqlDrop))(st => {
        log.debug(t.sqlDrop)
        st.executeUpdate
        infoMsgs += ("DROP TABLE " + t.objectName + ": OK")
      })(e => {
        errMsgs += ("DROP TABLE " + t.objectName + ": " + e.getMessage)
        log.trace("Error dropping table.", e)
      })

  def dropSchemata(conn: Connection) =
    for (s <- schemata)
      autoClose(conn.prepareStatement(s.sqlDrop))(st => {
        log.debug(s.sqlDrop)
        st.executeUpdate
        infoMsgs += ("DROP SCHEMA " + s.objectName + ": OK")
      })(e => {
        errMsgs += ("DROP SCHEMA " + s.objectName + ": " + e.getMessage)
        log.trace("Error dropping schema.", e)
      })

  def createSchemata(conn: Connection) =
    for (s <- schemata)
      autoClose(conn.prepareStatement(s.sqlCreate))(st => {
        log.debug(s.sqlCreate)
        st.executeUpdate
        infoMsgs += ("CREATE SCHEMA " + s.objectName + ": OK")
      })(e => {
        errMsgs += ("CRAETE SCHEMA " + s.objectName + ": " + e.getMessage)
        log.trace("Error creating schema.", e)
      })

  def createTables(conn: Connection) =
    for (t <- tables)
      autoClose(conn.prepareStatement(t.sqlCreate))(st => {
        log.debug(t.sqlCreate)
        st.executeUpdate
        infoMsgs += ("CREATE TABLE " + t.objectName + ": OK")
      })(e => {
        errMsgs += ("CREATE TABLE " + t.objectName + ": " + e.getMessage)
        log.trace("Error creating table.", e)
      })

  def createConstraints(conn: Connection) =
    for (c <- constraints)
      autoClose(conn.prepareStatement(c.sqlCreate))(st => {
        log.debug(c.sqlCreate)
        st.executeUpdate
        infoMsgs += ("CREATE CONSTRAINT " + c.objectName + ": OK")
      })(e => {
        errMsgs += ("CREATE CONSTRAINT " + c.objectName + ": " + e.getMessage)
        log.trace("Error creating constraint.", e)
      })

  def createSequences(conn: Connection) =
    for (s <- sequences)
      autoClose(conn.prepareStatement(s.sqlCreate))(st => {
        log.debug(s.sqlCreate)
        st.executeUpdate
        infoMsgs += ("CREATE SEQUENCE " + s.objectName + ": OK")
      })(e => {
        errMsgs += ("CREATE SEQUENCE " + s.objectName + ": " + e.getMessage)
        log.trace("Error creating sequence.", e)
      })

  def createViews(conn: Connection) =
    for (v <- views)
      autoClose(conn.prepareStatement(v.sqlCreate))(st => {
        log.debug(v.sqlCreate)
        st.executeUpdate
        infoMsgs += ("CREATE VIEW " + v.objectName + ": OK")
      })(e => {
        errMsgs += ("CREATE VIEW " + v.objectName + ": " + e.getMessage)
        log.trace("Error creating view.", e)
      })

  def createAuxiliaryObjects(conn: Connection) =
    for (o <- auxiliaryObjects)
      autoClose(conn.prepareStatement(o.sqlCreate))(st => {
        log.debug(o.sqlCreate)
        st.executeUpdate
        infoMsgs += ("CREATE OBJECT " + o.objectName + ": OK")
      })(e => {
        errMsgs += ("CREATE OBJECT " + o.objectName + ": " + e.getMessage)
        log.trace("Error creating auxiliary object.", e)
      })

}
