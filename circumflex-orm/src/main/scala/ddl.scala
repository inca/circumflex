package ru.circumflex.orm

import java.sql.Connection
import jdbc._
import ORM._

// ## DDL stuff

/**
 * A Unit-of-Work for generating database schema.
 */
class DDLUnit {
  import DDLUnit._

  // ### Objects

  protected var _schemata: Seq[Schema] = Nil
  def schemata = _schemata
  protected var _tables: Seq[Table[_]] = Nil
  def tables = _tables
  protected var _views: Seq[View[_]] = Nil
  def views = _views
  protected var _constraints: Seq[Constraint] = Nil
  def constraints = _constraints
  protected var _preAux: Seq[SchemaObject] = Nil
  def preAux = _preAux
  protected var _postAux: Seq[SchemaObject] = Nil
  def postAux = _postAux

  protected var _msgs: Seq[Msg] = Nil
  def messages = _msgs
  def msgsArray: Array[Msg] = messages.toArray

  def this(objects: SchemaObject*) = {
    this()
    add(objects: _*)
  }

  def resetMsgs(): this.type = {
    _msgs = Nil
    return this
  }

  def clear() = {
    _schemata = Nil
    _tables = Nil
    _views = Nil
    _constraints = Nil
    _preAux = Nil
    _postAux = Nil
    resetMsgs()
  }

  def add(objects: SchemaObject*): this.type = {
    objects.foreach(addObject(_))
    return this
  }

  def addObject(obj: SchemaObject): this.type = {
    def processRelation(r: Relation[_]) = {
      addObject(r.schema)
      r.preAux.foreach(o =>
        if (!_preAux.contains(o)) _preAux ++= List(o))
      r.postAux.foreach(o => addObject(o))
    }
    obj match {
      case t: Table[_] => if (!_tables.contains(t)) {
        _tables ++= List(t)
        t.constraints.foreach(o => addObject(o))
        processRelation(t)
      }
      case v: View[_] => if (!_views.contains(v)) {
        _views ++= List(v)
        processRelation(v)
      }
      case c: Constraint => if (!_constraints.contains(c))
        _constraints ++= List(c)
      case s: Schema => if (!_schemata.contains(s))
        _schemata ++= List(s)
      case o => if (!_postAux.contains(o))
        _postAux ++= List(o)
    }
    return this
  }

  // ### Workers

  protected def dropObjects(objects: Seq[SchemaObject], conn: Connection) =
    for (o <- objects.reverse)
      autoClose(conn.prepareStatement(o.sqlDrop))(st => {
        st.executeUpdate
        _msgs ++= List(InfoMsg("DROP "  + o.objectName + ": OK", o.sqlDrop))
      })(e =>
        _msgs ++= List(ErrorMsg("DROP " + o.objectName + ": " + e.getMessage, o.sqlDrop)))

  protected def createObjects(objects: Seq[SchemaObject], conn: Connection) =
    for (o <- objects)
      autoClose(conn.prepareStatement(o.sqlCreate))(st => {
        st.executeUpdate
        _msgs ++= List(InfoMsg("CREATE " + o.objectName + ": OK", o.sqlCreate))
      })(e =>
        _msgs ++= List(ErrorMsg("CREATE " + o.objectName + ": " + e.getMessage, o.sqlCreate)))

  /**
   * Execute a DROP script for added objects.
   */
  def drop(): this.type = {
    resetMsgs()
    _drop()
  }
  def _drop(): this.type = auto(tx.connection)(conn => {
    // We will commit every successfull statement.
    val autoCommit = conn.getAutoCommit
    conn.setAutoCommit(true)
    // Execute a script.
    dropObjects(postAux, conn)
    dropObjects(views, conn)
    if (dialect.supportsDropConstraints_?)
      dropObjects(constraints, conn)
    dropObjects(tables, conn)
    dropObjects(preAux, conn)
    if (dialect.supportsSchema_?)
      dropObjects(schemata, conn)
    // Restore auto-commit.
    conn.setAutoCommit(autoCommit)
    return this
  })

  /**
   * Execute a CREATE script for added objects.
   */
  def create(): this.type = {
    resetMsgs()
    _create()
  }
  def _create(): this.type = auto(tx.connection)(conn => {
    // We will commit every successfull statement.
    val autoCommit = conn.getAutoCommit
    conn.setAutoCommit(true)
    // Execute a script.
    if (dialect.supportsSchema_?)
      createObjects(schemata, conn)
    createObjects(preAux, conn)
    createObjects(tables, conn)
    createObjects(constraints, conn)
    createObjects(views, conn)
    createObjects(postAux, conn)
    // Restore auto-commit.
    conn.setAutoCommit(autoCommit)
    return this
  })

  /**
   * Execute a DROP script and then a CREATE script.
   */
  def dropCreate(): this.type = {
    resetMsgs()
    _drop()
    _create()
  }

  override def toString: String = {
    var result = "Circumflex DDL Unit: "
    if (messages.size == 0) {
        val objectsCount = (schemata.size +
            tables.size +
            constraints.size +
            views.size +
            preAux.size +
            postAux.size)
         result += objectsCount + " objects in queue."
    } else {
      val errorsCount = messages.filter(m => m.isInstanceOf[DDLUnit.ErrorMsg]).size
      val infoCount = messages.filter(m => m.isInstanceOf[DDLUnit.InfoMsg]).size
      result += infoCount + " successful statements, " + errorsCount + " errors."
    }
    return result
  }
}

// ### Messages

object DDLUnit {
  trait Msg {
    def body: String
    def sql: String
  }
  case class InfoMsg(val body: String, val sql: String) extends Msg
  case class ErrorMsg(val body: String, val sql: String) extends Msg
}
