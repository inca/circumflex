package ru.circumflex.orm

import ORM._
import ru.circumflex.core.Circumflex
import ru.circumflex.core.CircumflexUtil._
import collection.mutable.ListBuffer
import java.lang.reflect.Method

// ## Relations registry

/**
 * This singleton holds mappings between `Record` classes and their
 * corresponding relations. This provides weak coupling between the two
 * to allow proper initialization of either side.
 */
object RelationRegistry {

  protected var classToRelation: Map[Class[_], Relation[_]] = Map()

  def getRelation[R <: Record[R]](r: R): Relation[R] =
    classToRelation.get(r.getClass) match {
      case Some(rel: Relation[R]) => rel
      case _ => {
        val relClass = Circumflex.loadClass[Relation[R]](r.getClass.getName + "$")
        val relation = relClass.getField("MODULE$").get(null).asInstanceOf[Relation[R]]
        classToRelation += (r.getClass -> relation)
        relation
      }
    }

}

// ## Relation

abstract class Relation[R <: Record[R]] {

  // ### Commons

  // TODO add `protected[orm]` modifier
  val columns = new ListBuffer[Column]
  val constraints = new ListBuffer[Constraint]
  val preAux = new ListBuffer[SchemaObject]
  val postAux = new ListBuffer[SchemaObject]

  // Unique suffix is used to differentiate schema objects which have same names.
  private var _uniqueCounter = -1
  protected def uniqueSuffix: String = {
    _uniqueCounter += 1
    if (_uniqueCounter == 0) return ""
    else return "_" + _uniqueCounter
  }

  /**
   * Attempt to find a record class by convention of companion object,
   * e.g. strip trailing `$` from `this.getClass.getName`.
   */
  val recordClass: Class[R] = Circumflex
      .loadClass[R](this.getClass.getName.replaceAll("\\$(?=\\Z)", ""))

  /**
   * This sample is used to introspect record for columns, constraints and,
   * possibly, other stuff.
   */
  protected[orm] val recordSample: R = recordClass.newInstance

  /**
   * Relation name defaults to record's unqualified class name, transformed
   * with `Circumflex.camelCaseToUnderscore`.
   */
  def relationName = camelCaseToUnderscore(recordClass.getSimpleName)

  /**
   * Schema is used to produce a qualified name for relation.
   */
  val schema: String = defaultSchema

  /**
   * Obtain a qualified name for this relation from dialect.
   */
  def qualifiedName = dialect.relationQualifiedName(this)

  /**
   * Used to determine, whether DML statements are allowed against this relation.
   */
  def readOnly_?(): Boolean = false

  // ### Introspection and Initialization

  /**
   * Inspect `recordClass` to find columns and constraints definitions.
   */
  private def findMembers(cl: Class[_]): Unit = {
    if (cl != classOf[Any]) findMembers(cl.getSuperclass)
    cl.getDeclaredFields
        .filter(f => classOf[Field[R, _]].isAssignableFrom(f.getType))
        .map(f => cl.getMethod(f.getName))
        .foreach(m => processMember(m))
  }

  private def processMember(m: Method): Unit = m.getReturnType match {
    case cl if classOf[Field[R, _]].isAssignableFrom(cl) =>
      val col = new Column(this, m)
      this.columns += col
      if (col.unique_?) this.unique(col)
    case _ =>
  }

  findMembers(recordClass)

  // ### Definitions

  /**
   * Adds a unique constraint to this relation's definition.
   */
  protected[orm] def unique(columns: Column*): UniqueKey = {
    val constrName = relationName + "_" +
        columns.map(_.columnName).mkString("_") + "_key"
    val constr = new UniqueKey(this, constrName, columns.toList)
    this.constraints += constr
    return constr
  }

  protected[orm] def UNIQUE(columns: Column*) = unique(columns: _*)



}

// ## Table

class Table[R <: Record[R]] extends Relation[R]