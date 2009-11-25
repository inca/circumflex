package ru.circumflex.orm


import collection.mutable.HashSet
import java.sql.Connection
import org.slf4j.LoggerFactory

/**
 * Configuration is propagated to whatever extends this.
 */
trait Configurable {
  /**
   * Configuration object is used for all persistence-related stuff.
   * Override it if you want to use your own configuration implementation.
   * @return DefaultConfiguration by default
   */
  def configuration: Configuration = DefaultConfiguration

  def dialect = configuration.dialect
}

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
class Schema(val schemaName: String) extends SchemaObject {
  def sqlCreate = dialect.createSchema(this)

  def sqlDrop = dialect.dropSchema(this)

  override def equals(obj: Any) = obj match {
    case sc: Schema => sc.schemaName.equalsIgnoreCase(this.schemaName)
    case _ => false
  }

  override def hashCode = this.schemaName.toLowerCase.hashCode
}

/**
 * Default public schema singleton; used to avoid another abstract method on Table.
 */
object DefaultSchema extends Schema("public")

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

  /**
   * Adds a table to database objects list.
   */
  def addTable(tab: Table[_]): DDLExport = {
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
    autoClose(configuration.connectionProvider.getConnection)(conn => {
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
    autoClose(configuration.connectionProvider.getConnection)(conn => {
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
      autoClose(conn.prepareStatement(s.sqlDrop))(st => {
        st.executeUpdate
        log.debug(s.sqlDrop)
        log.info("DROP SEQUENCE {}: OK", s.sequenceName)
      })(e => {
        log.error("DROP SEQUENCE {}: {}", s.sequenceName, e.getMessage)
        log.trace("Error dropping sequence.", e)
      })
    }

  def dropConstraints(conn: Connection) =
    for (c <- constraints) {
      autoClose(conn.prepareStatement(c.sqlDrop))(st => {
        st.executeUpdate
        log.debug(c.sqlDrop)
        log.info("DROP CONSTRAINT {}: OK", c.constraintName)
      })(e => {
        log.error("DROP CONSTRAINT {}: {}", c.constraintName, e.getMessage)
        log.trace("Error dropping constraint.", e)
      })
    }

  def dropTables(conn: Connection) =
    for (t <- tables) {
      autoClose(conn.prepareStatement(t.sqlDrop))(st => {
        st.executeUpdate
        log.debug(t.sqlDrop)
        log.info("DROP TABLE {}: OK", t.tableName)
      })(e => {
        log.error("DROP TABLE {}: {}", t.tableName, e.getMessage)
        log.trace("Error dropping table.", e)
      })
    }

  def dropSchemata(conn: Connection) =
    for (s <- schemata) {
      autoClose(conn.prepareStatement(s.sqlDrop))(st => {
        st.executeUpdate
        log.debug(s.sqlDrop)
        log.info("DROP SCHEMA {}: OK", s.schemaName)
      })(e => {
        log.error("DROP SCHEMA {}: {}", s.schemaName, e.getMessage)
        log.trace("Error dropping schema.", e)
      })
    }

  def createSchemata(conn: Connection) =
    for (s <- schemata) {
      autoClose(conn.prepareStatement(s.sqlCreate))(st => {
        st.executeUpdate
        log.debug(s.sqlCreate)
        log.info("CREATE SCHEMA {}: OK", s.schemaName)
      })(e => {
        log.error("CRAETE SCHEMA {}: {}", s.schemaName, e.getMessage)
        log.trace("Error creating schema.", e)
      })
    }

  def createTables(conn: Connection) =
    for (t <- tables) {
      autoClose(conn.prepareStatement(t.sqlCreate))(st => {
        st.executeUpdate
        log.debug(t.sqlCreate)
        log.info("CREATE TABLE {}: OK", t.tableName)
      })(e => {
        log.error("CREATE TABLE {}: {}", t.tableName, e.getMessage)
        log.trace("Error creating table.", e)
      })
    }

  def createConstraints(conn: Connection) =
    for (c <- constraints) {
      autoClose(conn.prepareStatement(c.sqlCreate))(st => {
        st.executeUpdate
        log.debug(c.sqlCreate)
        log.info("CREATE CONSTRAINT {}: OK", c.constraintName)
      })(e => {
        log.error("CREATE CONSTRAINT {}: {}", c.constraintName, e.getMessage)
        log.trace("Error creating constraint.", e)
      })
    }

  def createSequences(conn: Connection) =
    for (s <- sequences) {
      autoClose(conn.prepareStatement(s.sqlCreate))(st => {
        st.executeUpdate
        log.debug(s.sqlCreate)
        log.info("CREATE SEQUENCE {}: OK", s.sequenceName)
      })(e => {
        log.error("CREATE SEQUENCE {}: {}", s.sequenceName, e.getMessage)
        log.trace("Error creating sequence.", e)
      })
    }

}