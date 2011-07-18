package ru.circumflex

import java.security.MessageDigest

/*!# The `core` Package

Package `core` contains different shortcuts, utilities and implicits.
You should import this package if you intend to use Circumflex API:

    import ru.circumflex.core._
*/
package object core {

  val CX_LOG = new Logger("ru.circumflex.core")

  val cx = Circumflex

  def ctx = Context.get()

  lazy val msg = cx.instantiate[MessageResolver]("cx.messages", new PropertyFileResolver)

  // Utils

  def camelCaseToUnderscore(arg: String) = arg.replaceAll("(?<!^)([A-Z])","_$1").toLowerCase

  def time[T](block: => T): (Long, T) = {
    val startTime = System.currentTimeMillis
    val result = block
    (System.currentTimeMillis - startTime, result)
  }

  def any2option[T](obj: T): Option[T] = if (obj == null) None else Some(obj)

  def digest(algorithm: String, text: String) = {
    val md = MessageDigest.getInstance(algorithm)
    md.update(text.getBytes)
    md.digest()
        .map(b => Integer.toHexString(0xFF & b))
        .map(b => if (b.length == 1) "0" + b else b)
        .mkString
  }
  def md5(text: String) = digest("md5", text)
  def sha256(text: String) = digest("sha-256", text)

  // Pimping Symbols

  @inline implicit def symbol2contextVarHelper(sym: Symbol): ContextVarHelper =
    new ContextVarHelper(sym)

}
