package ru.circumflex.orm

import ru.circumflex.core._

/*!# Exporting Database Schema

The `DDLUnit` class provides API for creating and dropping database schema.
It features arranging database objects in correct order (preliminary
auxiliary objects, tables, constraints, auxiliary database objects) and
configurable logging.
*/
class DDLUnit {

  protected var _schemata: Seq[Schema] = Nil
  def schemata = _schemata
  protected var _tables: Seq[Table[_, _]] = Nil
  def tables = _tables
  protected var _views: Seq[View[_, _]] = Nil
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
    this
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
    this
  }

  def addObject(obj: SchemaObject): this.type = {
    def processRelation(r: Relation[_, _]) {
      addObject(r.schema)
      r.preAux.foreach(o =>
        if (!_preAux.contains(o)) _preAux ++= List(o))
      r.postAux.foreach(o => addObject(o))
    }
    obj match {
      case t: Table[_, _] => if (!_tables.contains(t)) {
        _tables ++= List(t)
        t.constraints.foreach(c => addObject(c))
        t.indexes.foreach(i => addObject(i))
        processRelation(t)
      }
      case v: View[_, _] => if (!_views.contains(v)) {
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
    this
  }

  protected def dropObjects(objects: Seq[SchemaObject]) {
    objects.reverse.foreach { o =>
      tx.execute(o.sqlDrop, { st =>
        st.executeUpdate()
        _msgs ++= List(new Msg(
          "orm.ddl.info",
          "status" -> ("DROP " + o.objectName + ": OK"),
          "sql" -> o.sqlDrop))
      }, { e =>
        _msgs ++= List(new Msg(
          "orm.ddl.info",
          "status" -> ("DROP " + o.objectName + ": " + e.getMessage),
          "sql" -> o.sqlDrop,
          "error" -> e.getMessage))
      })
    }
  }

  protected def createObjects(objects: Seq[SchemaObject]) {
    objects.foreach { o =>
      tx.execute(o.sqlCreate, { st =>
        st.executeUpdate()
        _msgs ++= List(new Msg(
          "orm.ddl.info",
          "status" -> ("CREATE " + o.objectName + ": OK"),
          "sql" -> o.sqlCreate))
      }, { e =>
        _msgs ++= List(new Msg(
          "orm.ddl.error",
          "status" -> ("CREATE " + o.objectName + ": " + e.getMessage),
          "sql" -> o.sqlCreate,
          "error" -> e.getMessage))
      })
    }
  }

  def DROP(): this.type = {
    resetMsgs()
    _drop()
    this
  }

  def _drop() {
    tx.execute({ conn =>
    // We will commit every successfull statement.
      val autoCommit = conn.getAutoCommit
      conn.setAutoCommit(true)
      // Execute a script.
      dropObjects(postAux)
      dropObjects(views)
      if (dialect.supportsDropConstraints)
        dropObjects(constraints)
      dropObjects(tables)
      dropObjects(preAux)
      if (dialect.supportsSchema)
        dropObjects(schemata)
      // Restore auto-commit.
      conn.setAutoCommit(autoCommit)
    }, { throw _ })
  }

  def CREATE(): this.type = {
    resetMsgs()
    _create()
    this
  }

  def _create() {
    tx.execute({ conn =>
    // We will commit every successfull statement.
      val autoCommit = conn.getAutoCommit
      conn.setAutoCommit(true)
      // Execute a script.
      if (dialect.supportsSchema)
        createObjects(schemata)
      createObjects(preAux)
      createObjects(tables)
      createObjects(constraints)
      createObjects(views)
      createObjects(postAux)
      // Restore auto-commit.
      conn.setAutoCommit(autoCommit)
    }, { throw _ })
  }

  def DROP_CREATE(): this.type = {
    resetMsgs()
    _drop()
    _create()
    this
  }

  def close() {
    tx.close()
    connectionProvider.close()
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
      val infoCount = messages.filter(_.key == "orm.ddl.info").size
      val errorsCount = messages.filter(_.key == "orm.ddl.error").size
      result += infoCount + " successful statements, " + errorsCount + " errors."
    }
    result
  }
}