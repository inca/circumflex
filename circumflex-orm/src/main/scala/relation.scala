package ru.circumflex.orm

import ru.circumflex.core.Circumflex
import ru.circumflex.core.CircumflexUtil._

abstract class Relation[R <: Record[R]] {

  /**
   * Attempt to find a record class by convention of companion object,
   * e.g. strip trailing `$` from `this.getClass.getName`.
   */
  lazy val recordClass: Class[R] = {
    val className = this.getClass.getName.replaceAll("\\$(?=\\Z)","")
    Class.forName(className, true, Circumflex.classLoader).asInstanceOf[Class[R]]
  }

  def relationName = camelCaseToUnderscore(recordClass.getSimpleName)

}

class Table[R <: Record[R]] extends Relation[R]