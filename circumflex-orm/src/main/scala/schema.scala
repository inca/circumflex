package ru.circumflex.orm


import collection.mutable.HashSet
import java.io.Writer
import java.sql.Connection
import org.slf4j.LoggerFactory

/**
 * Defines a contract for database schema objects.
 * They must provide SQL statement to create them and to drop them.
 */
trait SchemaObject extends Configurable {
  /**
   * SQL statement to create this database object.
   */
  def sqlCreate: String

  /**
   * SQL statement to drop this database object.
   */
  def sqlDrop: String
}

/**
 * Base functionality for SQL schema.
 */
class Schema extends SchemaObject {
  def sqlCreate = dialect.createSchema(this)

  def sqlDrop = dialect.dropSchema(this)

  var schemaName = defaultSchemaName

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
class DDLExport extends Configurable
    with JDBCHelper {
  protected val log = LoggerFactory.getLogger("ru.circumflex.orm.ddl")

  private val schemata = HashSet[Schema]()
  private val tables = HashSet[Table[_]]()
  private val constraints = HashSet[Constraint[_]]()
  private val sequences = HashSet[Sequence[_]]()

  private val writers = HashSet[Writer]()

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
      dropSequences(conn)
      dropConstraints(conn)
      dropTables(conn)
      dropSchemata(conn)
    })(e => log.error("Connection failure occured while exporing DDL.", e))
  }

  def dropSequences(conn: Connection) =
    for (s <- sequences) {
      var msg = ""
      autoClose(conn.prepareStatement(s.sqlDrop))(st => {
        log.debug(s.sqlDrop)
        st.executeUpdate
        msg = "DROP SEQUENCE " + s.sequenceName + ": OK"
      })(e => {
        msg = "DROP SEQUENCE " + s.sequenceName + ": " + e.getMessage
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
        msg = "DROP CONSTRAINT " + c.constraintName + ": OK"
      })(e => {
        msg = "DROP CONSTRAINT " + c.constraintName + ": " + e.getMessage
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
        msg = "DROP TABLE " + t.tableName + ": OK"
      })(e => {
        msg = "DROP TABLE " + t.tableName + ": " + e.getMessage
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
        msg = "DROP SCHEMA " + s.schemaName + ": OK"
      })(e => {
        msg = "DROP SCHEMA " + s.schemaName + ": " + e.getMessage
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
        msg = "CREATE SCHEMA " + s.schemaName + ": OK"
      })(e => {
        msg = "CRAETE SCHEMA " + s.schemaName + ": " + e.getMessage
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
        msg = "CREATE TABLE " + t.tableName + ": OK"
      })(e => {
        msg = "CREATE TABLE " + t.tableName + ": " + e.getMessage
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
        msg = "CREATE CONSTRAINT " + c.constraintName + ": OK"
      })(e => {
        msg = "CREATE CONSTRAINT " + c.constraintName + ": " + e.getMessage
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
        msg = "CREATE SEQUENCE " + s.sequenceName + ": OK"
      })(e => {
        msg = "CREATE SEQUENCE " + s.sequenceName + ": " + e.getMessage
        log.trace("Error creating sequence.", e)
      })
      log.info(msg)
      write(msg)
    }

}