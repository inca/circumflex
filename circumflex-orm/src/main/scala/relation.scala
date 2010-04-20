package ru.circumflex.orm

import ru.circumflex.core.Circumflex
import ru.circumflex.core.CircumflexUtil._

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

class Table[R <: Record[R]] extends Relation[R]