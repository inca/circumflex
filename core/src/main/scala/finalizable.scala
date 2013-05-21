package circumflex
package core

/*! `Finalizable` trait adds abstract finalization functionality
based on collecting functions `() => Unit` in current context.

The implementation then decides when to perform such finalization
(i.e. execute all the functions).
*/
trait Finalizable {

  protected def baseName: String

  private def _fnkey = baseName + ".finalizers"

  def finalizers = ctx.getAs[Seq[() => Unit]](_fnkey).getOrElse(Nil)

  def enqueueFinalizer(key: String, fn: () => Unit) {
    if (!ctx.contains(_fnkey + "." + key)) {
      ctx.update(_fnkey + "." + key, true)
      ctx.update(_fnkey, finalizers ++ Seq(fn))
    }
  }

  def pushFinalizer(key: String, fn: () => Unit) {
    if (!ctx.contains(_fnkey + "." + key)) {
      ctx.update(_fnkey + "." + key, true)
      ctx.update(_fnkey, Seq(fn) ++ finalizers)
    }
  }

  def doFinalize() {
    finalizers.foreach(_.apply())
  }

}