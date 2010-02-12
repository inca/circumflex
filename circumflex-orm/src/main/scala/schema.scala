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
import org.apache.commons.beanutils.MethodUtils

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
   * SQL object name. It is used to uniquely identify this object
   * during schema creation by <code>DDLExport</code> to avoid duplicates.
   * Object names are case-insensitive (e.g. "MY_TABLE" and "my_table" are
   * considered equal).
   */
  def objectName: String

  override def hashCode = objectName.toLowerCase.hashCode

  override def equals(obj: Any) = obj match {
    case so: SchemaObject => so.objectName.equalsIgnoreCase(this.objectName)
    case _ => false
  }

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
  val preAuxiliaryObjects = new ListBuffer[SchemaObject]()
  val postAuxiliaryObjects = new ListBuffer[SchemaObject]()

  val writers = new HashSet[Writer]()
  val loggers = HashSet[Any](log)

  val infoMsgs = new ListBuffer[String]()
  val errMsgs = new ListBuffer[String]()

  private var _debugToWriters = false;

  def this(objList: SchemaObject*) = {
    this()
    objList.toList.foreach(addObject(_))
  }

  def addWriter(writer: Writer): this.type = {
    this.writers += writer
    return this
  }

  def addLogger(l: Any): this.type = {
    this.loggers += l
    return this
  }

  def debugToWriters(value: Boolean): this.type = {
    _debugToWriters = value
    return this
  }

  def info(msg: String) = {
    writers.foreach(_.write(msg + "\n"))
    loggers.foreach(l => try {
      MethodUtils.invokeMethod(l, "info", msg)
    } catch {
      case _ => log.trace("Could not invoke info(String) method on supplied log.")
    })
  }

  def debug(msg: String) = {
    if (_debugToWriters) writers.foreach(_.write(msg + "\n"))
    loggers.foreach(l => try {
      MethodUtils.invokeMethod(l, "debug", msg)
    } catch {
      case _ => log.trace("Could not invoke debug(String) method on supplied log.")
    })
  }

  def error(msg: String, e: Throwable) = {
    writers.foreach(_.write(msg + ": " + e.getMessage))
    loggers.foreach(l => try {
      MethodUtils.invokeMethod(l, "error", Array(msg, e))
    } catch {
      case _ => log.trace("Could not invoke error(String, Throwable) method on supplied log.")
    })
  }

  def addObject(obj: SchemaObject): this.type = {
    obj match {
      case t: Table[_] => {
        tables += t
        addObject(t.schema)
        t.constraints.foreach(o => addObject(o))
        t.preAuxiliaryObjects.foreach(o =>
          if (!preAuxiliaryObjects.contains(o))
            preAuxiliaryObjects += o)
        t.postAuxiliaryObjects.foreach(o => addObject(o))
      }
      case v: View[_] => {
        views += v
        addObject(v.schema)
        v.preAuxiliaryObjects.foreach(o =>
          if (!preAuxiliaryObjects.contains(o))
            preAuxiliaryObjects += o)
        v.postAuxiliaryObjects.foreach(o => addObject(o))
      }
      case c: Constraint[_] => constraints += c
      case s: Schema => schemata += s
      case o => if (!postAuxiliaryObjects.contains(o)) postAuxiliaryObjects += o
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
    autoClose(tx.connection)(conn => {
      // we will commit every successful statement
      val autoCommit = conn.getAutoCommit
      conn.setAutoCommit(true)
      // clear statistics
      infoMsgs.clear
      errMsgs.clear
      // process database objects
      info("Executing schema create script.")
      if (dialect.supportsSchema_?)
        createSchemata(conn)
      createPreAuxiliaryObjects(conn)
      createTables(conn)
      createConstraints(conn)
      createViews(conn)
      createPostAuxiliaryObjects(conn)
      // restore previous auto-commit setting
      conn.setAutoCommit(autoCommit)
      // report log and statistics
      infoMsgs.foreach(info(_))
      errMsgs.foreach(info(_))
      info("Create schema script finished.")
      info(infoMsgs.size + " statements executes successfully.")
      info(errMsgs.size + " statements failed.")
    })(e => error("Connection failure occured while exporing DDL.", e))

  /**
   * Executes DROP script.
   */
  def drop: Unit =
    autoClose(tx.connection)(conn => {
      // we will commit every successful statement
      val autoCommit = conn.getAutoCommit
      conn.setAutoCommit(true)
      // clear statistics
      infoMsgs.clear
      errMsgs.clear
      // process database objects
      info("Executing schema drop script.")
      dropPostAuxiliaryObjects(conn)
      dropViews(conn)
      if (dialect.supportDropConstraints_?)
        dropConstraints(conn)
      dropTables(conn)
      dropPreAuxiliaryObjects(conn)
      if (dialect.supportsSchema_?)
        dropSchemata(conn)
      // restore previous auto-commit setting
      conn.setAutoCommit(autoCommit)
      // report log and statistics
      infoMsgs.foreach(info(_))
      errMsgs.foreach(info(_))
      info("Drop schema script finished.")
      info(infoMsgs.size + " statements executes successfully.")
      info(errMsgs.size + " statements failed.")
    })(e => error("Connection failure occured while exporing DDL.", e))

  def dropPreAuxiliaryObjects(conn: Connection) =
    for (o <- preAuxiliaryObjects.reverse)
      autoClose(conn.prepareStatement(o.sqlDrop))(st => {
        debug(o.sqlDrop)
        st.executeUpdate
        infoMsgs += ("DROP OBJECT " + o.objectName + ": OK")
      })(e => {
        errMsgs += ("DROP OBJECT " + o.objectName + ": " + e.getMessage)
        log.trace("Error dropping auxiliary object.", e)
      })

  def dropPostAuxiliaryObjects(conn: Connection) =
    for (o <- postAuxiliaryObjects.reverse)
      autoClose(conn.prepareStatement(o.sqlDrop))(st => {
        debug(o.sqlDrop)
        st.executeUpdate
        infoMsgs += ("DROP OBJECT " + o.objectName + ": OK")
      })(e => {
        errMsgs += ("DROP OBJECT " + o.objectName + ": " + e.getMessage)
        log.trace("Error dropping auxiliary object.", e)
      })

  def dropViews(conn: Connection) =
    for (v <- views) {
      autoClose(conn.prepareStatement(v.sqlDrop))(st => {
        debug(v.sqlDrop)
        st.executeUpdate
        infoMsgs += ("DROP VIEW " + v.objectName + ": OK")
      })(e => {
        errMsgs += ("DROP VIEW " + v.objectName + ": " + e.getMessage)
        log.trace("Error dropping view.", e)
      })
    }

  def dropConstraints(conn: Connection) =
    for (c <- constraints)
      autoClose(conn.prepareStatement(c.sqlDrop))(st => {
        debug(c.sqlDrop)
        st.executeUpdate
        infoMsgs += ("DROP CONSTRAINT " + c.objectName + ": OK")
      })(e => {
        errMsgs += ("DROP CONSTRAINT " + c.objectName + ": " + e.getMessage)
        log.trace("Error dropping constraint.", e)
      })

  def dropTables(conn: Connection) =
    for (t <- tables)
      autoClose(conn.prepareStatement(t.sqlDrop))(st => {
        debug(t.sqlDrop)
        st.executeUpdate
        infoMsgs += ("DROP TABLE " + t.objectName + ": OK")
      })(e => {
        errMsgs += ("DROP TABLE " + t.objectName + ": " + e.getMessage)
        log.trace("Error dropping table.", e)
      })

  def dropSchemata(conn: Connection) =
    for (s <- schemata)
      autoClose(conn.prepareStatement(s.sqlDrop))(st => {
        debug(s.sqlDrop)
        st.executeUpdate
        infoMsgs += ("DROP SCHEMA " + s.objectName + ": OK")
      })(e => {
        errMsgs += ("DROP SCHEMA " + s.objectName + ": " + e.getMessage)
        log.trace("Error dropping schema.", e)
      })

  def createSchemata(conn: Connection) =
    for (s <- schemata)
      autoClose(conn.prepareStatement(s.sqlCreate))(st => {
        debug(s.sqlCreate)
        st.executeUpdate
        infoMsgs += ("CREATE SCHEMA " + s.objectName + ": OK")
      })(e => {
        errMsgs += ("CRAETE SCHEMA " + s.objectName + ": " + e.getMessage)
        log.trace("Error creating schema.", e)
      })

  def createTables(conn: Connection) =
    for (t <- tables)
      autoClose(conn.prepareStatement(t.sqlCreate))(st => {
        debug(t.sqlCreate)
        st.executeUpdate
        infoMsgs += ("CREATE TABLE " + t.objectName + ": OK")
      })(e => {
        errMsgs += ("CREATE TABLE " + t.objectName + ": " + e.getMessage)
        log.trace("Error creating table.", e)
      })

  def createConstraints(conn: Connection) = {
    def create(constrs: Iterable[Constraint[_]]) =
      for (c <- constrs)
        autoClose(conn.prepareStatement(c.sqlCreate))(st => {
          debug(c.sqlCreate)
          st.executeUpdate
          infoMsgs += ("CREATE CONSTRAINT " + c.objectName + ": OK")
        })(e => {
          errMsgs += ("CREATE CONSTRAINT " + c.objectName + ": " + e.getMessage)
          log.trace("Error creating constraint.", e)
        })
    create(constraints.filter(!_.isInstanceOf[ForeignKey[_, _]]))
    create(constraints.filter(_.isInstanceOf[ForeignKey[_, _]]))
  }

  def createViews(conn: Connection) =
    for (v <- views)
      autoClose(conn.prepareStatement(v.sqlCreate))(st => {
        debug(v.sqlCreate)
        st.executeUpdate
        infoMsgs += ("CREATE VIEW " + v.objectName + ": OK")
      })(e => {
        errMsgs += ("CREATE VIEW " + v.objectName + ": " + e.getMessage)
        log.trace("Error creating view.", e)
      })

  def createPreAuxiliaryObjects(conn: Connection) =
    for (o <- preAuxiliaryObjects)
      autoClose(conn.prepareStatement(o.sqlCreate))(st => {
        debug(o.sqlCreate)
        st.executeUpdate
        infoMsgs += ("CREATE OBJECT " + o.objectName + ": OK")
      })(e => {
        errMsgs += ("CREATE OBJECT " + o.objectName + ": " + e.getMessage)
        log.trace("Error creating auxiliary object.", e)
      })

  def createPostAuxiliaryObjects(conn: Connection) =
    for (o <- postAuxiliaryObjects)
      autoClose(conn.prepareStatement(o.sqlCreate))(st => {
        debug(o.sqlCreate)
        st.executeUpdate
        infoMsgs += ("CREATE OBJECT " + o.objectName + ": OK")
      })(e => {
        errMsgs += ("CREATE OBJECT " + o.objectName + ": " + e.getMessage)
        log.trace("Error creating auxiliary object.", e)
      })

}
