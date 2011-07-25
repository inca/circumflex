package ru.circumflex
package orm

import core._
import java.io.File
import java.lang.IllegalStateException
import org.apache.commons.io.FileUtils
import collection.JavaConversions._
import org.apache.commons.io.filefilter.IOFileFilter
import java.lang.reflect.Modifier

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
      if (ormConf.dialect.supportsDropConstraints)
        dropObjects(constraints)
      dropObjects(tables)
      dropObjects(preAux)
      if (ormConf.dialect.supportsSchema)
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
      if (ormConf.dialect.supportsSchema)
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
    ormConf.connectionProvider.close()
  }

  def objectsCount: Int = schemata.size +
    tables.size +
    constraints.size +
    views.size +
    preAux.size +
    postAux.size

  override def toString: String = {
    var result = "Circumflex DDL Unit: "
    if (messages.size == 0) {
      result += objectsCount + " objects in queue."
    } else {
      val infoCount = messages.filter(_.key == "orm.ddl.info").size
      val errorsCount = messages.filter(_.key == "orm.ddl.error").size
      result += infoCount + " successful statements, " + errorsCount + " errors."
    }
    result
  }
}

/*!# Building Schema from Sources

The `DDLUnit` singleton can inspect your compiled classes to find the relations and build
schema from them. The usage is pretty simple:

    DDLUnit.fromClasspath().CREATE()

You can also specify package prefix for searching (using either slashes or dots as delimiters):

    DDLUnit.fromClasspath("com.myapp.model").CREATE()

By default, the compiled classes are being searched in `target/classes` and `target/test-classes`
directories relative to your project's root. You can override this setting using `cx.build.outputDirs`
configuration parameter (paths are split from String using `File.pathSeparator`, i.e. colon ":" in UNIX
and ";" in Windows).

Actual resolving is performed using context class loader of current thread (obtained via
`Thread.currentThread.getContextClassLoader`) so that you can override it in any time.
*/
object DDLUnit {

  def outputDirs: Iterable[File] = cx.get("cx.build.outputDirs") match {
    case Some(i: Iterable[File]) => i
    case Some(s: String) => s.split(File.pathSeparator).map(s => new File(s))
    case _ => List(new File("target/classes"), new File("target/test-classes"))
  }

  def fromClasspath(pkgPrefix: String = ""): DDLUnit = {
    val loader = Thread.currentThread.getContextClassLoader
    val ddl = new DDLUnit()
    for (dir <- outputDirs) {
      try {
        // Resolve directories and paths
        val pkgPath = pkgPrefix.replaceAll("\\.", "/")
        val outUrl = dir.toURI.toURL
        val pkgUrl = loader.getResource(pkgPath)
        val classDir = new File(outUrl.getFile, pkgPath)
        ORM_LOG.debug("Looking for schema objects in " + classDir.getAbsolutePath)
        // Make sure that requisite paths exist
        if (pkgUrl == null)
          throw new IllegalStateException("Could not resolve package '" + pkgPath + "'")
        if (!classDir.isDirectory)
          throw new IllegalStateException("Class directory " + classDir.getAbsolutePath + " does not exist.")
        // Iterate class files
        val files = FileUtils.listFiles(classDir, Array("class"), true).asInstanceOf[java.util.Collection[File]]
        for (file <- collectionAsScalaIterable(files)) {
          val relPath = file.getCanonicalPath.substring(dir.getCanonicalPath.length + 1)
          val className = relPath.substring(0, relPath.length - ".class".length).replaceAll(File.separator, ".")
          // Ensure that anonymous objects are not processed separately.
          if (className.matches("[^\\$]+(?:\\$$)?"))
            try {
              val c = loader.loadClass(className)
              var so: SchemaObject = null
              // try to treat it as a singleton
              try {
                val module = c.getField("MODULE$")
                if (isSchemaObjectType(module.getType))
                  so = module.get(null).asInstanceOf[SchemaObject]
              } catch {
                case e: NoSuchFieldException =>
                  // Try to instantiate it as a POJO.
                  if (isSchemaObjectType(c))
                    so = c.newInstance.asInstanceOf[SchemaObject]
              }
              if (so != null) {
                ddl.addObject(so)
                ORM_LOG.debug("Found schema object: " + c.getName)
              }
            } catch {
              case e: Exception =>
              // Omit non-schema classes silently
            }
        }
      } catch {
        case e: Exception => ORM_LOG.warn(e.getMessage)
      }
    }
    ORM_LOG.debug("Lookup complete, " + ddl.objectsCount + " objects found.")
    ddl
  }

  protected def isSchemaObjectType(c: Class[_]): Boolean =
    classOf[SchemaObject].isAssignableFrom(c) &&
      !Modifier.isAbstract(c.getModifiers) &&
      !Modifier.isInterface(c.getModifiers)

}