package ru.circumflex

import ru.circumflex.core._

/*!# The `orm` Package

Package `orm` contains different shortcuts, utilities, helpers and implicits --
the basis of DSL of Circumflex ORM.

You should import this package to use Circumflex ORM in your application:

    import ru.circumflex.orm._
*/
package object orm {

  val ORM_LOG = new Logger("ru.circumflex.orm")

  object jdbc {
    def autoClose[A <: {def close(): Unit}, B](obj: => A)
                                              (actions: A => B)
                                              (errors: Throwable => B): B =
      try {
        val res = obj
        try {
          return actions(res)
        } catch {
          case e => return errors(e)
        } finally {
          res.close
        }
      } catch {
        case e => return errors(e)
      }
  }

}