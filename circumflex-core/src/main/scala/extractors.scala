package ru.circumflex.core

/* Extractors composer (Scala trick!) */
object & {
  def unapply[A](a: A) = Some(a -> a)
}

/* ## Types extractors (String => A) */

object Int {
 def unapply(s: String): Option[Int] =
   try { Integer.parseInt(s) }
   catch { case _: NumberFormatException => None }
}

// TODO: other standard converters (Double, Date, ...)