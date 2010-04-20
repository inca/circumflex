package ru.circumflex.orm

import ru.circumflex.core.Circumflex
import ru.circumflex.core.CircumflexUtil._

/* ## Relations registry */

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

/* ## Relation */

abstract class Relation[R <: Record[R]] {

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
  protected val recordSample: R = recordClass.newInstance

  /**
   * Relation name defaults to record's unqualified class name, transformed
   * with `Circumflex.camelCaseToUnderscore`.
   */
  def relationName = camelCaseToUnderscore(recordClass.getSimpleName)

}

/* ## Table */

class Table[R <: Record[R]] extends Relation[R]